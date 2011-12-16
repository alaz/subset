package com.osinka.subset

import com.mongodb.{BasicDBObjectBuilder, DBObject}
import Values._

abstract class Subset(val name: String)(implicit outer: Path) extends Path {
  override val path: List[String] = outer.path :+ name
  implicit val myPath: Path = this

  def apply(flist: Serializer*): Serializer =
    Serializer.writer[DBObject](name, flist reduceLeft {_ ~ _} get)

  def from(dbo: DBObject): Option[DBObject] = Serializer.read[DBObject](name, dbo)

  // TODO: $elemMatch
  def elemMatch(f: this.type => Serializer): Serializer = {
    f(this)
  }

  // TODO: positional $
  def update(f: this.type => Serializer): Serializer = {
    f(this)
  }

  override def toString: String = "Subset("+longName+")"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Subset => super.equals(other) }
}