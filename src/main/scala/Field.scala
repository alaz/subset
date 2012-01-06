/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.osinka.subset

import com.mongodb.DBObject
import query._
import update._
import DBObjectLens._
import QueryLens._

/** A typed field
  * 
  * == Serialization ==
  * The first of all, a field provides serialization/deserialization capabilities.
  *
  * It's always possible to apply a field to its type and get a
  * [[com.osinka.subset.DBObjectLens]] as a result
  * {{{
  * val f = Field[Int]("a")
  * val lens = f(10)
  * }}}
  *
  * Any field is an extractor as well. It accepts `DBObject` and returns
  * `Option[T]`, so that one may write
  * {{{
  * dbo match {
  *   case Field(value) => ...
  * }
  * }}}
  *
  * == Lenses ==
  * `Field` is a source for a number of lenses (see [[com.osinka.subset.DBObjectLens]])
  *
  * The simplest one is that sets a key to some value
  * {{{
  * val f = "f".fieldOf[Int]
  * val lens = f(10)
  *
  * val newDbo: DBObject = lens.get
  * val modifiedDbo: DBObject = lens :~> dbo
  * }}}
  *
  * here, in the example, `newDbo` will be `{f: 10}` (because `get` applies a lens to an empty
  * `DBObject`), but `modifiedDbo` may have a lot of fields, though `lens` makes sure the only
  * one, `f`, gets set to `10`.
  *
  * There is a lens that removes a key
  * {{{
  * val lens = -f
  * val newDbo = lens :~> exisingDbo
  * }}}
  * if ever `existingDbo` had a key `f`, `newDbo` will not have it.
  *
  * "Modifier" lens mutates an existing key value. The result may be of another type,
  * so this operation depends both on [[com.osinka.subset.ValueReader]] and
  * [[com.osinka.subset.ValueWriter]] type classes:
  * {{{
  * val lens = f.update{i => (i+1).toString}
  * val newDbo = lens :~> existingDbo
  * }}}
  *
  * Thus, if `existingDbo` contains a key `f` which can be read as an `Int`, it get
  * transformed and written back under the same key.
  * 
  * == Tuples ==
  * '''Subset''' provides a kind of Tuple serializers for reading and
  * writing `TupleN` to/from `DBObject`
  *
  * As soon as you join two fields with a `~` method, you build a
  * [[com.osinka.subset.Tuple2Subset]], suitable for serializing tuples:
  * {{{
  * val T2 = "int".fieldOf[Int] ~ "str".fieldOf[String]
  * val lens = t( 10 -> "str" )
  *
  * dbo match {
  *   case T2(i, s) => ...
  * }
  * }}}
  *
  * You may create tuples of higher arity:
  * {{{
  * val T3 = T2 ~ "bool".fieldOf[Boolean]
  * val dbo: DBObject = T3( (10, "str", false) )
  * }}}
  * 
  * == Querying ==
  * A field is where query terms get created.
  *
  * You may create queries on the fields, e.g. 'equality test' (`=== 5`), 'less than' (`< 5`), etc. See
  * [[com.osinka.subset.query.Query]]
  *
  * == Update operators ==
  * A field provides methods to create update operations. See [[com.osinka.subset.update.Update]]
  * 
  * == Cloning fields ==
  * 
  * === Modifying Types ===
  * There is a number of typical field types that can be of help.
  *
  * `Int` field (created by `int` method) is usually used in MongoDB to declare
  * sorting and indexes:
  * {{{
  * val userName = "uname".fieldOf[String]
  * collection.ensureIndex(userName.int === 1, userName.int === -1)
  * }}}
  *
  * `Any` field (created by `any` method) may be helpful to write or read a
  * field "as is". If you absolutely certain in what you are doing,
  * {{{
  * val userName = "uname".fieldOf[String]
  * collection.modify(userName === "john", userName.any.set(10566))
  * }}}
  *
  * === Modifying name & scope ===
  * It is possible to create a "positional" field, that may be used
  * to update the first matched element in an array (in [[com.osinka.subset.update.Update]]
  * operations), see
  * [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]]
  * for details.
  *
  * E.g. assuming `seq` is a field of `Seq[Int]`
  * {{{
  * collection.modify(seq > 3, seq.first inc -1)
  * }}}
  * creates update modifier `{\$inc: {"seq.\$": -1}}`.
  *
  * A field representing an array element at index `i` is created with `field.at(i)`
  * {{{
  * collection.modify(Query.empty, seq.at(2) set 5)
  * }}}
  * creates update modifier `{\$set: {"seq.2": 5}}`.
  *
  * === Subset ===
  * Sometimes a subset's field can be used so frequently, that it makes sense to
  * create a field alias. The idea is to avoid repetiting code like
  * {{{
  * val query = subset.where{_.field === 10}
  * }}}
  *
  * and instead write
  * {{{
  * val alias = field.in(subset)
  * val query = alias === 10
  * }}}
  *
  * @tparam T is a type of the field.
  * @see [[com.osinka.subset.DBObjectLens]], [[com.osinka.subset.ValueReader]],
  *      [[com.osinka.subset.ValueWriter]], [[com.osinka.subset.Subset]]
  */
class Field[T](override val path: List[String]) extends Path with FieldConditions[T] with Modifications[T] {
  field =>

  //
  // Mutators
  //
  
  /** Create a new field, that has the same name and scope, but with another type.
   */
  def as[A]: Field[A] = new Field[A](path)

  /** Create a new field, that has the same name and scope, but `Int` type
   * 
   * `Field[Int]` is of much help to produce `[Int]` queries (in `\$special`, `\$maxKey`, `\$minKey`, sort, index, etc.)
   */
  def int: Field[Int] = as[Int]

  /** Create a new field, that has the same name and scope, but `Any` type.
   * 
   * `Field[Any]` is of much help to insert custom objects or e.g. `org.bson.types.{MaxKey, MinKey}`
   */
  def any: Field[Any] = as[Any]

  /** Create a field representing array element
    *
    * @param index an index of array element
    *
    * @see [[http://www.mongodb.org/display/DOCS/Dot+Notation+(Reaching+into+Objects)#DotNotation%28ReachingintoObjects%29-ArrayElementbyPosition Array element by position]]
    */
  def at(index: Int) = new Field[T](path :+ index.toString)

  /** Create a new positional field with the same type. The last element is "$"
   *
   * Creates a positional field to update the first matched element in an array.
   * 
   * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]]
   */
  def matched = new Field[T](path :+ "$")

  /** Create an alias
    */
  def in(subset: Field[_]): Field[T] = new Field[T]( (subset + this).path )

  /**Create a tuple subset
   */
  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

  //
  // Queries / Update modifiers
  //
  
  /**Create a query relative to this field
    */
  def where(q: Query): Query = Query( wrap(this, q.queryLens) )

  /** Creates a query as an \$elemMatch relative to this document
    *
    * @see [[http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch Advanced Queries - elemMatch]]
    */
  def elemMatch(q: Query): Query = Query( write(this, writer("$elemMatch", q.get)) )

  /** Creates an update operator positioned relative to this document
    *
    * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator Updating - The positional operator]]
    */
  def modify(u: Update): Update = u.copy(ops = u.ops mapValues {wrap(this, _)})

  /**`\$pull` based on a condition
    */
  def pullWhere(q: Query)(implicit ev: T <:< Traversable[_]): Update = op("$pull", q.get)

  //
  // Lenses
  //
  
  /** Writer lens
    */
  def apply(x: T)(implicit setter: ValueWriter[T]): DBObjectLens = writer(name, x)

  /** (an alias for `drop`)
    */
  def unary_- : DBObjectLens = remove

  /** Removing lens
   */
  def remove: DBObjectLens = DBObjectLens.remover(name)

  /**Lens modifying a value
    */
  def update[R](f: T => R)(implicit r: ValueReader[T], w: ValueWriter[R]): DBObjectLens = DBObjectLens.modifier(name, f)

  def unapply(dbo: DBObject)(implicit getter: ValueReader[T]): Option[T] = read[T](name, dbo)

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Field[_] => super.equals(other) }

  override def prefixString: String = "Field"
}

object Field {
  /** Field factory method
    */
  def apply[T](name: String): Field[T] = new Field[T](name :: Nil)
}
