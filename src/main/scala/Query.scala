package com.osinka.subset

import java.util.regex.Pattern
import util.matching.Regex
import com.mongodb.{DBObject, QueryOperators}
import QueryOperators._
import RichDBO._
import Implicits._

object Conditions {
  implicit def stringTupleSerializer[T : ValueWriter](t: (String, T)): Serializer = Serializer(_.write(t._1, t._2).get)
}

import Conditions._

trait Conditions[T] extends Address {
  protected def aquery(condition: Serializer): AddressQuery[T]

  def exists(v: Boolean) = aquery(EXISTS -> v)
  def >(x: T)(implicit writer: ValueWriter[T]) = aquery(GT -> x)
  def >=(x: T)(implicit writer: ValueWriter[T]) = aquery(GTE -> x)
  def <(x: T)(implicit writer: ValueWriter[T]) = aquery(LT -> x)
  def <=(x: T)(implicit writer: ValueWriter[T]) = aquery(LTE -> x)
  def !==(x: T)(implicit writer: ValueWriter[T]) = aquery(NE -> x)
  def size(n: Int) = aquery(SIZE -> n) // TODO: for Seq[T] only
  def `type`(n: Int) = aquery("$type" -> n)
  def in(s: Seq[T])(implicit writer: ValueWriter[Seq[T]]) = aquery(IN -> s) // TODO: for Seq[T] only
  def notIn(s: Seq[T])(implicit writer: ValueWriter[Seq[T]]) = aquery(NIN -> s) // TODO: for Seq[T] only
  def all(s: Seq[T])(implicit writer: ValueWriter[Seq[T]]) = aquery(ALL -> s) // TODO: for Seq[T] only
}

trait FieldConditions[T] extends Conditions[T] {
  override def aquery(condition: Serializer) = AddressQuery[T](name, longName, condition)

  def ===(x: T)(implicit writer: ValueWriter[T]) = Query(longName -> x)
  def ===(x: Option[T])(implicit writer: ValueWriter[T]): Query = x map {this ===} getOrElse exists(false)
  def ===(x: Regex)(implicit writer: ValueWriter[Regex]) = Query(longName -> x) // TODO: for String only
  def ===(x: Pattern) = Query( (dbo: DBObject) => dbo.write(longName, x).get  ) // TODO: for String only
}

trait Query extends Serializer {
  def and(other: Query): Query = Query(this ~ other)
  def &&(other: Query): Query = and(other)

  override def prefixString = "Query"
}

object Query {
  private case class DefaultImpl(override val write: DBObject => DBObject) extends Query

  def apply(f: Serializer): Query = DefaultImpl(f.write)
}

case class AddressQuery[T](override val name: String, override val longName: String, val condition: Serializer) extends Query with Conditions[T] {
  import ValueWriter.defaultWriter

  override def aquery(cond: Serializer) = copy(condition = condition ~ cond)

  override def write: DBObject => DBObject =
    (dbo: DBObject) => dbo.write(longName, condition.write(empty))

  override def prefixString = "AddressQuery"
}