package com.osinka.subset

import com.mongodb.{DBObject,BasicDBObjectBuilder}

case class Builder(val set: (Path, DBObject) => DBObject) {
  def andThen(other: Builder) =
    new Builder((p: Path, dbo: DBObject) => other.set(p, set(p, dbo)))
}

object Builder {
  def empty =
    (_: Path, _: DBObject) => BasicDBObjectBuilder.start.get

  def init(dbo: DBObject) =
    (_: Path, _: DBObject) => dbo

  def writer[T](x: T)(implicit w: ValueWriter[T]) =
    (p: Path, dbo: DBObject) => {
      w.pack(x) foreach { dbo.put(p.longName, _) }
      dbo
    }

  def writer[T](p: Path, x: T)(implicit w: ValueWriter[T]): (Path, DBObject) => DBObject =
    (_: Path, dbo: DBObject) => writer[T](x)(w)(p, dbo)

  def relative(scope: Path)(b: => Builder): Builder =
    new Builder( (p: Path, dbo: DBObject) => b.set(p.relative(scope), dbo) )

  def apply() = empty
  def apply(dbo: DBObject) = init(dbo)
  def apply[T](x: T)(implicit w: ValueWriter[T]) = writer[T](x)
  def apply[T](p: Path, x: T)(implicit w: ValueWriter[T]) = writer[T](p, x)
}