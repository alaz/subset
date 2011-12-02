package com.osinka.subset

import com.mongodb.{DBObject, BasicDBObjectBuilder}
import RichDBO._
/*
object QueryOp {
  def gt(f: DBObject => DBObject) =
    (dbo: DBObject) => dbo.write("$gt", f(empty))
}

trait Conditions[T] {
  def ==?(x: T)(implicit setter: Setter[T]): Condition[T]
  def >?(x: T)(implicit setter: Setter[T]): Condition[T]
}

case class Condition[T](val field: Field[T], val f: DBObject => DBObject) extends Conditions[T] {
  def ==?(x: T)(implicit setter: Setter[T]) = copy(f = field(x)(setter))
  def >?(x: T)(implicit setter: Setter[T]) = copy(f = f andThen QueryOp.gt(field(x)(setter)))
}

trait QueryConversions {
  def conditionToDBO(cond: Condition[_]): DBObject =
    empty.write(cond.field.longName, cond.f(empty))
}
*/