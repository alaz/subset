package com.osinka.subset

import com.mongodb.DBObject
import RichDBO._

object Implicits extends Implicits

trait Implicits {
  // default collection-level scope
  implicit val topLevelPath: Path = Path.empty

  // String to Field
  implicit def stringToField(name: String) =
    new AnyRef {
      def fieldOf[T](implicit outer: Path): Field[T] = Field[T](name)(outer)
    }

  // String Tuple
  implicit def stringTupleSerializer[T : ValueWriter](t: (String, T)): Serializer = Serializer(_.write(t._1, t._2).get)

  // Field conversions
  implicit def fieldTupleSerializer[T : ValueWriter](t: (Field[T], T)): Serializer = Serializer(_.write(t._1.name, t._2).get)
  implicit def fieldTupleDBO[T : ValueWriter](t: (Field[T], T)): DBObject = serializerToDBO( fieldTupleSerializer[T](t) )

  // Serializer
  implicit def dboToSerializer(f: DBObject => DBObject) = Serializer(f)
  implicit def serializerToDBO(s: Serializer): DBObject = s.get

  // Update
  implicit def updateToDBO(u: Update): DBObject = u.get

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
