package com.osinka.subset

import com.mongodb.{DBObject,BasicDBObjectBuilder}

// Ordinary lens, modifying an DBObject
trait Lens extends (DBObject => DBObject) {
  def get: DBObject = apply(BasicDBObjectBuilder.start.get)

  def ~ (other: Lens): Lens = Lens(this andThen other)

  def prefixString = "Lens"

  override def toString: String = prefixString+get

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Lens if prefixString == other.prefixString => get == other.get }

  override def hashCode: Int = get.hashCode
}

object Lens {
  def apply(f: DBObject => DBObject): Lens =
    new Lens {
      def apply(dbo: DBObject): DBObject = f(dbo)
    }

  implicit def fToLens(f: DBObject => DBObject): Lens = apply(f)
  implicit def lensWriter: ValueWriter[Lens] = ValueWriter[Lens](_.get)

  def read[T](key: String, dbo: DBObject)(implicit reader: ValueReader[T]): Option[T] =
    Option(dbo.get(key)) flatMap {reader.unpack(_)}

  def writer[T](key: String, x: T)(implicit w: ValueWriter[T]): Lens =
    (dbo: DBObject) => {
        w.pack(x) foreach {dbo.put(key, _)}
        dbo
      }
}

import Lens._

// Query lenses are little bit more complex, they get a "scope" parameter as well
trait QueryLens extends (Path => Lens) {
  def ~(other: QueryLens): QueryLens =
    QueryLens( (p: Path) => this(p) ~ other(p) )
}

object QueryLens {
  def apply(f: Path => Lens): QueryLens =
    new QueryLens {
      def apply(p: Path): Lens = f(p)
    }

  implicit def fToQLens(f: Path => Lens): QueryLens = apply(f)

  def relative[T : ValueWriter](p: Path, x: T): QueryLens =
    (scope: Path) => writer(p.relativeTo(scope).longName, x)

  def positional[T : ValueWriter](p: Path, x: T): QueryLens =
    (scope: Path) => writer(p.positionIn(scope).longName, x)

  def embed(s: String, ql: Traversable[QueryLens]): QueryLens =
    (scope: Path) => writer(s, ql map {_.apply(scope).get} toArray)

  def embed(s: String, ql: QueryLens): QueryLens =
    (scope: Path) => writer(s, ql(scope))

  def embed(p: Path, ql: QueryLens): QueryLens =
    (scope: Path) => writer(p.relativeTo(scope).longName, ql(scope))
}
