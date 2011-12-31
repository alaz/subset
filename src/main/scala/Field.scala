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

/** A typed field
  * 
  * == Serialization ==
  * The first of all, a field provides serialization/deserialization capabilities.
  *
  * It's always possible to apply a field to its type and get a [[com.osinka.subset.DBObjectLens]]
  * as a result
  * {{{
  * val f = Field[Int]("a")
  * val lens = f(10)
  * }}}
  *
  * Any field is an extractor as well. It accepts `DBObject` and returns `Option[T]`, so that it's possible
  * to write
  * {{{
  * dbo match {
  *   case Field(value) => ...
  * }
  * }}}
  *
  * == Tuples ==
  * It is possible to build Tuple serializers from several fields.
  *
  * As soon as you join two fields with a `~` method, you build a [[com.osinka.subset.Tuple2Subset]] ,
  * suitable for serializing tuples:
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
  * `Int` field (created by `int` method) is usually used in MongoDB to declare sorting and indexes.
  * {{{
  * val userName = "uname".fieldOf[String]
  * collection.ensureIndex(userName.int === 1, userName.int === -1)
  * }}}
  *
  * `Any` field (created by `any` method) may be helpful to write or read a field "as is". If you absolutely certain in what
  * you are doing,
  * {{{
  * val userName = "uname".fieldOf[String]
  * collection.update(userName === "john", userName.any.set(10566))
  * }}}
  *
  * === Modifying name & scope ===
  * It is possible to create a "positional" field, that may be used
  * to update the first matched element in an array (in [[com.osinka.subset.update.Update]] operations),
  * see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]] for
  * details.
  * {{{
  * collection.update( Comments.By === "joe", Comments.Votes.first.in(Comments).inc(1) )
  * }}}
  *
  * Analogously, in the case a query or update modifier need to be created for an element of array,
  * {{{
  * collection.find( Comments.Votes.at(0).in(Comments) === "joe" )
  * }}}
  *
  * === Subset ===
  * When you create a field, it gets attached to the enclosing scope (determined via an implicit). Though you certainly
  * may explicitly create a field in the scope you'd like, there are a couple of helper methods to clone the fields
  * you like to the outer (collection) scope or any subdocument:
  * {{{
  * val userName = "uname".fieldOf[String]
  *
  * object PostCreateEventOwner extends Subset[DBObject]("postAuthor") {
  *   val author = userName.attach
  * }
  *
  * val author = PostCreateEventOwner.author.detach
  * }}}
  * 
  * See the longer example in [[com.osinka.subset.Subset]].
  *
  * @see [[com.osinka.subset.DBObjectLens]], [[com.osinka.subset.ValueReader]], [[com.osinka.subset.ValueWriter]], [[com.osinka.subset.Subset]]
  */
class Field[T](val name: String)(implicit outer: Path = Path.empty) extends Path with FieldConditions[T] with Modifications[T] {
  field =>

  override val path: List[String] = outer.path :+ name

  /** Create a new field attached to the scope
   */
  def attach(implicit scope: Path): Field[T] = new Field[T](name)(scope)

  /** Create a new field detached from the scope
   */
  def detach: Field[T] = new Field[T](name)(Path.empty)

  /** Create a new field, that has the same name and scope, but with another type.
   */
  def as[A]: Field[A] = new Field[A](name)(outer)

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

  class PositionalField private[Field] (override val name: String) extends Field[T](name)(this) {
    /** Move the position to another scope.
     *
     * Creates a positional field relative to the scope specified
     *
     * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]],
     *      [[http://www.mongodb.org/display/DOCS/Dot+Notation+(Reaching+into+Objects)#DotNotation%28ReachingintoObjects%29-ArrayElementbyPosition Array element by position]]
     */
    def in(scope: Path): Field[T] = {
      val p = field.positionIn(scope, name)
      new Field[T](p.path.last)(Path(p.path dropRight 1))
    }
  }

  /** Create a field representing array element
    *
    * @param index an index of array element
    *
    * @see [[http://www.mongodb.org/display/DOCS/Dot+Notation+(Reaching+into+Objects)#DotNotation%28ReachingintoObjects%29-ArrayElementbyPosition Array element by position]]
    */
  def at(index: Int) = new PositionalField(index.toString)

  /** Create a new positional field with the same type. The last element is "$"
   *
   * Creates a positional field to update the first matched element in an array.
   * 
   * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]]
   */
  def first = new PositionalField("$")

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

  def apply(x: T)(implicit setter: ValueWriter[T]): DBObjectLens = writer(name, x)

  def unapply(dbo: DBObject)(implicit getter: ValueReader[T]): Option[T] = read[T](name, dbo)

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Field[_] => super.equals(other) }

  override def toString: String = "Field "+longName
}

object Field {
  /** Field factory method
    */
  def apply[T](name: String)(implicit outer: Path = Path.empty): Field[T] = new Field[T](name)
}