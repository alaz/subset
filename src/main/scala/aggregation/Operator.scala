package com.osinka.subset
package aggregation

import com.mongodb.DBObject

class Operator(val v: Any)

/**
 * Aggregation framework operators for "project"
 *
 * http://docs.mongodb.org/manual/reference/aggregation/#aggregation-operators
 */
object Operator {
  def apply[T : ValueWriter](v: T) = new Operator(ValueWriter.pack(v).get)
  def apply[T : ValueWriter](s: String, v: T): Operator = apply(Mutation.writer(s, v) : DBObject)

  implicit val fieldWriter = ValueWriter[Field[_]](_.projection)
}
