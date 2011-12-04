package com.osinka.subset

import com.mongodb.{DBObject, BasicDBObjectBuilder,QueryOperators}
import RichDBO._
import Implicits._

object Conditions {
  implicit def stringTupleSerializer[T : ValueWriter](t: (String, T)): Serializer = Serializer(_.write(t._1, t._2))

  def notEqual[T] = Field[T](QueryOperators.NE)
  def greaterThan[T] = Field[T](QueryOperators.GT)
  def greaterThanOrEqual[T] = Field[T](QueryOperators.GTE)
}

import Conditions._

trait Conditions[T] extends Address {
  def >(x: T)(implicit writer: ValueWriter[T]): AddressQuery[T] = AddressQuery[T](name, longName, greaterThan(x))
  def >=(x: T)(implicit writer: ValueWriter[T]): AddressQuery[T] = AddressQuery[T](name, longName, greaterThanOrEqual(x))
  def !==(x: T)(implicit writer: ValueWriter[T]): AddressQuery[T] = AddressQuery[T](name, longName, notEqual(x))
}

trait FieldConditions[T] extends Conditions[T] {
  def ===(x: T)(implicit writer: ValueWriter[T]): Query = Query(longName -> x)
}

trait Query extends Serializer {
  def &&(other: Query): Query = Query(this ~ other)
}

object Query {
  private case class DefaultImpl(override val write: DBObject => DBObject) extends Query

  def apply(f: Serializer): Query = DefaultImpl(f.write)
}

case class AddressQuery[T](override val name: String, override val longName: String, val condition: Serializer) extends Query with Conditions[T] {
  import ValueWriter.defaultWriter

  override def write: DBObject => DBObject =
    (dbo: DBObject) => dbo.write(longName, condition.write(empty))

  override def >(x: T)(implicit writer: ValueWriter[T]) = copy(condition = condition ~ greaterThan[T](x))
  override def >=(x: T)(implicit writer: ValueWriter[T]) = copy(condition = condition ~ greaterThanOrEqual[T](x))
}