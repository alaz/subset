package com.osinka.subset
package aggregation

case class Operator[T](m: Mutation)

/**
 * Aggregation framework operators for "project"
 *
 * http://docs.mongodb.org/manual/reference/aggregation/#aggregation-operators
 */
object Operator {
  private def op[T](s: String, f: Field[T]) = new Operator[T](Mutation.writer(s, f.projection))

  def toLower[T](f: Field[T]) = op[T]("$toLower", f)
  def toUpper[T](f: Field[T]) = op[T]("$toUpper", f)
  // TODO: other operators
}
