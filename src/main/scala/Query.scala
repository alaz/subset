package com.osinka.subset

import com.mongodb.{DBObject, BasicDBObjectBuilder}
import RichDBO._
/*
trait Conditions[T] extends Address {
  def ==?(x: T): Query
  def >?(x: T) = FieldQuery[T](name, longName, ??)
}

trait Query {
  def write: DBObject => DBObject

  def &&(other: Query): Query = Query(write andThen other.write)
}

object Query {
  case class DefaultImpl(override val write: DBObject => DBObject) extends Query

  def apply(dbo: DBObject): Query = apply(_ => dbo)
  def apply(f: DBObject => DBObject): Query = DefaultImpl(f)
}

case class FieldQuery[T](override val name: String, override val longName: String, override val write: DBObject => DBObject)(implicit writer: ValueWriter[T]) extends Query with Conditions[T]

trait QueryConversions {
  implicit def fieldToConditions[T](f: Field[T])(implicit writer: ValueWriter[T]) = FieldQuery[T](f.name, f.longName, identity _)
}
*/