package com.osinka.subset

import java.util.regex.Pattern
import util.matching.Regex
import com.mongodb.{DBObject, QueryOperators}
import Implicits._

private[subset] object Conditions {
  val NOT    = "$not"
  val TYPE   = "$type"
  val WITHIN = "$within"
  val CENTER = "$center"
  val BOX    = "$box"
  val OR     = "$or"
  val NOR    = "$nor"
  val WHERE  = "$where"
  val AND    = "$and"
}

// importing Op names
import QueryOperators._
import Conditions._

trait Conditions[T] extends Path {
  protected def aquery(condition: Serializer): AddressQuery[T]

  def exists(v: Boolean) = aquery(EXISTS -> v)
  def >(x: T)(implicit writer: ValueWriter[T]) = aquery(GT -> x)
  def >=(x: T)(implicit writer: ValueWriter[T]) = aquery(GTE -> x)
  def <(x: T)(implicit writer: ValueWriter[T]) = aquery(LT -> x)
  def <=(x: T)(implicit writer: ValueWriter[T]) = aquery(LTE -> x)
  def !==(x: T)(implicit writer: ValueWriter[T]) = aquery(NE -> x)
  def mod(by: Int, rest: Int)(implicit writer: ValueWriter[Traversable[Int]]) = aquery(MOD -> List(by, rest))
  def size(n: Int) = aquery(SIZE -> n) // TODO: for Seq[T] only
  def `type`(n: Int) = aquery(TYPE -> n)
  def in(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = aquery(IN -> s) // TODO: for Seq[T] only
  def notIn(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = aquery(NIN -> s) // TODO: for Seq[T] only
  def all(s: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = aquery(ALL -> s) // TODO: for Seq[T] only

  def near(x: Double, y: Double)(implicit writer: ValueWriter[Traversable[Double]]) = aquery(NEAR -> List(x, y))
  def near(x: Double, y: Double, d: Double)(implicit writer: ValueWriter[Traversable[Double]]) = aquery(NEAR -> List(x, y, d))
  def withinCenter(x: Double, y: Double, r: Double) =
    aquery(WITHIN -> Serializer.writer(CENTER, Array( Array(x, y), r)).get)
  def withinBox(x: Double, y: Double, x2: Double, y2: Double) =
    aquery(WITHIN -> Serializer.writer(BOX, Array(Array(x,y), Array(x2,y2)) ).get)
}

trait FieldConditions[T] extends Conditions[T] {
  override def aquery(condition: Serializer) = AddressQuery[T](this, condition)

  def ===(x: T)(implicit writer: ValueWriter[T]) = Query(longName -> x)
  def ===(x: Option[T])(implicit writer: ValueWriter[T]): Query = x map {this ===} getOrElse exists(false)
  def ===(x: Regex)(implicit writer: ValueWriter[Regex]) = Query(longName -> x) // TODO: for String only
  def ===(x: Pattern) = Query( Serializer.writer(longName, x)  ) // TODO: for String only
}

trait Query extends Serializer {
  def &&(other: Query): Query = and(other)

  def and(other: Query): Query = {
    import collection.JavaConversions._

    if ( (this.get.keySet & other.get.keySet).isEmpty )
      Query(this ~ other)
    else
      AndQuery(other :: this :: Nil)
  }

  def ||(other: Query) = or(other)
  def or(other: Query): Query = OrQuery(other :: this :: Nil)

  def nor(other: Query): Query = NorQuery(other :: this :: Nil)

  override def prefixString = "Query"
}

case class AndQuery(val queries: List[Query]) extends Query {
  override def and(other: Query): Query = copy(queries = other :: queries)

  override def write: DBObject => DBObject =
    Serializer.writer(AND, queries.reverse map{_.get} toArray) write
}

case class OrQuery(val queries: List[Query]) extends Query {
  override def or(other: Query): Query = copy(queries = other :: queries)

  override def write: DBObject => DBObject =
    Serializer.writer(OR, queries.reverse map{_.get} toArray) write
}

case class NorQuery(val queries: List[Query]) extends Query {
  override def nor(other: Query): Query = copy(queries = other :: queries)

  override def write: DBObject => DBObject =
    Serializer.writer(NOR, queries.reverse map{_.get} toArray) write
}

object Query {
  private case class DefaultImpl(override val write: DBObject => DBObject) extends Query

  def apply(f: Serializer): Query = DefaultImpl(f.write)
}

case class AddressQuery[T](val p: Path, val condition: Serializer) extends Query with Conditions[T] {
  override def path: List[String] = p.path

  def not : Query = Query( Serializer.writer(longName, Serializer.writer(NOT, condition.get).get) )
  def unary_! = not

  override def aquery(cond: Serializer) = copy(condition = condition ~ cond)

  override def write: DBObject => DBObject =
    Serializer.writer(longName, condition.get) write

  override def prefixString = "AddressQuery"
}