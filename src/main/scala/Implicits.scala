package com.osinka.subset

import com.mongodb.DBObject
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

  // Serializer
  implicit def dboToSerializer(f: (DBObject => DBObject)) = Serializer(f)
  implicit def serializerToDBO(s: Serializer): DBObject = s.write(empty)

  // Tuple conversions
  implicit def fieldTupleSerializer[T : ValueWriter](t: (Field[T], T)): Serializer = Serializer(_.write(t._1.name, t._2))
  implicit def fieldToDBO[T : ValueWriter](t: (Field[T], T)): DBObject = serializerToDBO(fieldTupleSerializer(t))

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
