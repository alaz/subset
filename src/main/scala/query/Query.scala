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
package query

import java.util.regex.Pattern
import util.matching.Regex
import com.mongodb.DBObject

import DBObjectLens._
import QueryLens._

/** All the field conditions that MongoDB allows to verify.
  */
trait Conditions[T] extends Path {
  protected def fquery(condition: DBObjectLens): FieldQuery[T]

  def exists(v: Boolean) = fquery("$exists" -> v)
  def >(x: T)(implicit writer: ValueWriter[T]) = fquery("$gt" -> x)
  def >=(x: T)(implicit writer: ValueWriter[T]) = fquery("$gte" -> x)
  def <(x: T)(implicit writer: ValueWriter[T]) = fquery("$lt" -> x)
  def <=(x: T)(implicit writer: ValueWriter[T]) = fquery("$lte" -> x)
  def !==(x: T)(implicit writer: ValueWriter[T]) = fquery("$ne" -> x)
  def mod(by: Int, rest: Int)(implicit writer: ValueWriter[Traversable[Int]]) = fquery("$mod" -> List(by, rest))
  def size(n: Int)(implicit ev: T <:< Traversable[_]) = fquery("$size" -> n)
  def `type`(n: Int) = fquery("$type" -> n)
  def in(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = fquery("$in" -> s)
  def notIn(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = fquery("$nin" -> s)
  def all[A](s: Traversable[A])(implicit writer: ValueWriter[Traversable[A]], ev: T <:< Traversable[A]) = fquery("$all" -> s)

  def near(x: Double, y: Double)(implicit writer: ValueWriter[Traversable[Double]]) =
    fquery("$near" -> List(x, y))
  def near(x: Double, y: Double, d: Double)(implicit writer: ValueWriter[Traversable[Double]]) =
    fquery("$near" -> List(x, y, d))
  def withinCenter(x: Double, y: Double, r: Double) =
    fquery("$within" -> writer("$center", Array( Array(x, y), r)).get )
  def withinBox(x: Double, y: Double, x2: Double, y2: Double) =
    fquery("$within" -> writer("$box", Array(Array(x,y), Array(x2,y2)) ).get )
}

/** Equality condition for a field.
  *
  * This trait mixes into [[com.osinka.subset.Field]]
  * 
  * Equality condition is applicable for fields only and can be expressed in
  * terms of several methods:
  *  - `"f".fieldOf[Int] === 1` creates an ordinary equality test `{f: 1}`
  *  - `"f".fieldOf[Int] === Some(1)` does the same, while
  *  - `"f".fieldOf[Int] === None` creates `{f: {\$exists: false}}`
  *  - `"f".fieldOf[String] === "regexp".r` (you may supply Java's `Pattern` as well)
  *    creates regexp matching operator `{f: /regexp/}`
  */
trait FieldConditions[T] extends Conditions[T] {
  override def fquery(condition: DBObjectLens) = FieldQuery[T](this, condition)

  def ===(x: T)(implicit writer: ValueWriter[T]) = Query(this, x)
  def ===(x: Option[T])(implicit writer: ValueWriter[T]): Query = x map {this ===} getOrElse exists(false)
  def ===(x: Regex)(implicit writer: ValueWriter[Regex]): Query = Query(this, x) // TODO: for String only
  def ===(x: Pattern): Query = Query(this, x) // TODO: for String only
}

/** A Query
  *
  * == Lens ==
  * Query is a [[com.osinka.subset.DBObjectLens]], which means you may
  * get a `DBObject` or apply it to the existing `DBObject` at any time.
  * 
  * Thus whenever you have a `Query`, you may get its MongoDB repesentation
  * explicitly using `get`
  * {{{
  * val q = f > 5 < 10
  * collection.findOne( q.get )
  * }}}
  * 
  * or implicitly
  * 
  * {{{
  * collection.findOne(q : DBObject)
  * }}}
  * 
  * '''NOTE:''' `DBCollection.findOne` is a method that accepts `AnyRef`, that's
  * why an implicit conversion will not be triggered automatically and you have
  * to say explicitly you want to convert a `Query` into `DBObject`
  * 
  * == Composition ==
  * Queries allow composition using methods
  *  - `&&` (the same as `and`), this will result in `\$and`
  *  - `||` (the same as `or`), this will result in `\$or`
  *  - `nor`, this will result in `\$nor`
  * 
  * Whenever you join queries with `&&`, '''Subset''' tries to determine if
  * both queries use the same keys. If they does, it creates an `\$and` query.
  * If not, it simply joins two maps.
  * 
  * {{{
  * val q1 = (f > 0 && f < 10).get
  * }}}
  * This results in `{\$and: [{f: {\$gt:0}}, {f: {\$lt:10}}]}`.
  * 
  * {{{
  * val q2 = (f > 0 && g < 10).get
  * }}}
  * This results in `{f: {\$gt:0}, g: {\$lt:10}}`
  * 
  * The same rules apply to the larger number of query terms.
  * 
  * @see [https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]
  */
trait Query extends DBObjectLens {
  def queryLens: QueryLens

  override def apply(dbo: DBObject): DBObject = queryLens(Path.empty)(dbo)

  def &&(other: Query): Query = and(other)
  def and(other: Query): Query = {
    import collection.JavaConversions._

    if ( (get.keySet & other.get.keySet).isEmpty ) Query(queryLens ~ other.queryLens)
    else Query.And(other :: this :: Nil)
  }

  def ||(other: Query) = or(other)
  def or(other: Query): Query = Query.Or(other :: this :: Nil)

  def nor(other: Query): Query = Query.Nor(other :: this :: Nil)

  override def prefixString = "Query"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Query => super.equals(other) }
}

object Query {
  /** @return an empty query
    */
  def empty: Query = const(DBObjectLens.empty)

  def const(dbo: DBObject): Query = const(DBObjectLens const dbo)
  def const(lens: DBObjectLens): Query = apply(QueryLens {_: Path => lens})

  def apply[T : ValueWriter](p: Path, x: T): Query = apply(write(p, x))
  def apply(ql: QueryLens): Query = DefaultImpl(ql)

  private case class DefaultImpl(override val queryLens: QueryLens) extends Query

  private[subset] case class And(queries: List[Query]) extends Query {
    override def and(other: Query): Query = copy(queries = other :: queries)
    override def queryLens: QueryLens = embed("$and", queries map {_.queryLens} reverse)
  }

  private[subset] case class Or(queries: List[Query]) extends Query {
    override def or(other: Query): Query = copy(queries = other :: queries)
    override def queryLens: QueryLens = embed("$or", queries map {_.queryLens} reverse)
  }

  private[subset] case class Nor(queries: List[Query]) extends Query {
    override def nor(other: Query): Query = copy(queries = other :: queries)
    override def queryLens: QueryLens = embed("$nor", queries map {_.queryLens} reverse)
  }
}

/** A query term that helps define several conditions on the same field (e.g. ranges)
  * 
  * The example shows how it looks like:
  * {{{
  * val f = "f".fieldOf[Int]
  * val query1: DBObject = f >= 2 < 10 in List(1,4,7,12)
  * }}}
  * The result would be `{f: {\$gte: 2, \$lt: 10, \$in: [1,4,7,12]}`.
  * 
  * There is a negation operator as well, e.g.
  * {{{
  * val query2: DBObject = !( f >= 2 < 10 )
  * }}}
  * The result would be `{f: {\$not: {\$gte: 2, \$lt: 10}}}`
  */
case class FieldQuery[T](p: Path, condition: DBObjectLens) extends Query with Conditions[T] {
  override def path: List[String] = p.path

  override def fquery(cond: DBObjectLens) = copy(condition = condition andThen cond)

  override def queryLens: QueryLens = write(p, condition)

  def not : Query = Query(write(p, writer("$not", condition)))
  def unary_! = not

  override def prefixString = "FieldQuery"
}