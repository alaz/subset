package com.osinka

package object subset extends RecoveringPrimitivesSerializer with JodaTimeSerializer {
  // default collection-level scope
  implicit val collectionScope: Scope = Scope.CollectionLevel

  // String to Field
  implicit def stringToField(name: String) =
    new AnyRef {
      def fieldOf[T](implicit scope: Scope): Field[T] = Field[T](name)(scope)
    }

  // Tuple to DBO
  // List[Tuple] to DBO

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