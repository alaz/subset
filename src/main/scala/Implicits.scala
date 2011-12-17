package com.osinka.subset

import com.mongodb.DBObject

object Implicits extends Implicits

trait Implicits {
//  // default collection-level scope
//  implicit val topLevelScope: Path = Path.empty

  // String to Field
  implicit def stringToField(name: String) =
    new AnyRef {
      def fieldOf[T](implicit outer: Path = Path.empty): Field[T] = Field[T](name)(outer)
    }

  // String Tuple
  implicit def stringTupleSerializer[T : ValueWriter](t: (String, T)): Lens = Lens.writer(t._1, t._2)

  // Field conversions
  implicit def fieldTupleSerializer[T : ValueWriter](t: (Field[T], T)): Lens = Lens.writer(t._1.name, t._2)
  implicit def fieldTupleDBO[T : ValueWriter](t: (Field[T], T)): DBObject = fieldTupleSerializer[T](t).get

  // Lenses
  implicit def lensToDBO(l: Lens): DBObject = l.get
  implicit def fToLens(f: DBObject => DBObject): Lens = Lens.fToLens(f)
  implicit def fToQLens(f: Path => Lens): QueryLens = QueryLens.fToQLens(f)

  // Update
  implicit def updateToLens(u: Update)(implicit scope: Path = Path.empty): Lens = u.get(scope)
  implicit def updateToDBO(u: Update)(implicit scope: Path = Path.empty): DBObject = lensToDBO(u.get(scope))

  // Few pimps
  implicit def enrichDBO(dbo: DBObject) =
    new AnyRef {
      def %(lens: Lens): DBObject = lens(dbo)
    }

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
