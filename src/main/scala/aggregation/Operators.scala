package com.osinka.subset
package aggregation

/**
 * Aggregation framework field operators
 */
case class Operator[T](m: Mutation)

object Operator {
  def toLower[T](f: Field[T]) = new Operator[T](Mutation.writer("$toLower", f.projection))
  def toUpper[T](f: Field[T]) = new Operator[T](Mutation.writer("$toUpper", f.projection))
  // TODO: lots of similar methods
}
