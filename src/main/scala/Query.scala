package com.osinka.subset

import java.util.regex.Pattern
import util.matching.Regex
import com.mongodb.DBObject

import Implicits._
import Lens._
import QueryLens._

trait Conditions[T] extends Path {
  protected def fquery(condition: Lens): FieldQuery[T]

  def exists(v: Boolean) = fquery("$exists" -> v)
  def >(x: T)(implicit writer: ValueWriter[T]) = fquery("$gt" -> x)
  def >=(x: T)(implicit writer: ValueWriter[T]) = fquery("$gte" -> x)
  def <(x: T)(implicit writer: ValueWriter[T]) = fquery("$lt" -> x)
  def <=(x: T)(implicit writer: ValueWriter[T]) = fquery("$lte" -> x)
  def !==(x: T)(implicit writer: ValueWriter[T]) = fquery("$ne" -> x)
  def mod(by: Int, rest: Int)(implicit writer: ValueWriter[Traversable[Int]]) = fquery("$mod" -> List(by, rest))
  def size(n: Int) = fquery("$size" -> n) // TODO: for Seq[T] only
  def `type`(n: Int) = fquery("$type" -> n)
  def in(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = fquery("$in" -> s) // TODO: for Seq[T] only
  def notIn(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = fquery("$nin" -> s) // TODO: for Seq[T] only
  def all(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = fquery("$all" -> s) // TODO: for Seq[T] only

  def near(x: Double, y: Double)(implicit writer: ValueWriter[Traversable[Double]]) =
    fquery("$near" -> List(x, y))
  def near(x: Double, y: Double, d: Double)(implicit writer: ValueWriter[Traversable[Double]]) =
    fquery("$near" -> List(x, y, d))
  def withinCenter(x: Double, y: Double, r: Double) =
    fquery("$within" -> writer("$center", Array( Array(x, y), r)).get )
  def withinBox(x: Double, y: Double, x2: Double, y2: Double) =
    fquery("$within" -> writer("$box", Array(Array(x,y), Array(x2,y2)) ).get )
}

trait FieldConditions[T] extends Conditions[T] {
  override def fquery(condition: Lens) = FieldQuery[T](this, condition)

  def ===(x: T)(implicit writer: ValueWriter[T]) = Query(this, x)
  def ===(x: Option[T])(implicit writer: ValueWriter[T]): Query = x map {this ===} getOrElse exists(false)
  def ===(x: Regex)(implicit writer: ValueWriter[Regex]): Query = Query(this, x) // TODO: for String only
  def ===(x: Pattern): Query = Query(this, x) // TODO: for String only
}

trait Query extends Lens {
  def queryLens: QueryLens

  override def apply(dbo: DBObject): DBObject = queryLens(topLevelScope)(dbo)

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
  def apply[T : ValueWriter](p: Path, x: T): Query = apply(relative(p, x))
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


case class FieldQuery[T](p: Path, condition: Lens) extends Query with Conditions[T] {
  override def path: List[String] = p.path

  override def fquery(cond: Lens) = copy(condition = condition andThen cond)

  override def queryLens: QueryLens = relative(p, condition)

  def not : Query = Query(relative(p, writer("$not", condition)))
  def unary_! = not

  override def prefixString = "FieldQuery"
}