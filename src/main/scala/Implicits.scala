package com.osinka.subset

import RichDBO._

object Implicits extends Implicits

trait Implicits extends JodaTimePacking {
  // default collection-level scope
  implicit val collectionScope: Scope = Scope.CollectionLevel

  // String to Field
  implicit def stringToField(name: String) =
    new AnyRef {
      def fieldOf[T](implicit scope: Scope): Field[T] = Field[T](name)(scope)
    }

  // Tuple conversions
  implicit def fieldTupleWriter[T : ValueWriter] =
    ValueWriter[(Field[T], T)](t => empty.write(t._1.name, t._2))

  /**
   * Conjunction for use in pattern matching
   *
   * Based on idea from
   * http://stackoverflow.com/questions/2261358/pattern-matching-with-conjunctions-patterna-and-patternb
   */
  object ~ {
    def unapply[A](a: A) = Some((a,a))
  }
}
