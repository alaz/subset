package com.osinka.subset
package aggregation

import com.mongodb.DBObject

class Operator(val v: Any)

object Operator {
  case class Val(v: Any)

  object Val {
    implicit val writer = ValueWriter[Val](_.v)
  }

  def apply[T : ValueWriter](v: T) = new Operator(ValueWriter.pack(v).get)
  def apply(s: String, args: Val*): Operator =
    args.toList match {
      case v :: Nil =>
        apply(Mutation.writer(s, v) : DBObject)
      case list =>
        apply(Mutation.writer(s, list) : DBObject)
    }

  implicit val fieldWriter = ValueWriter[Field[_]](_.projection)
}
