package com.osinka.subset

import com.mongodb.DBObject

import Lens._
import QueryLens._

abstract class Subset(val name: String)(implicit outer: Path) extends Path {
  override val path: List[String] = outer.path :+ name
  implicit val myPath: Path = this

  def apply(flist: Lens*): Lens = writer(name, flist reduceLeft {_ ~ _})

  def from(dbo: DBObject)(implicit r: ValueReader[DBObject]): Option[DBObject] = read[DBObject](name, dbo)

  // TODO: $elemMatch
  def elemMatch(f: this.type => Query): Query = Query(relative(this, f(this).queryLens(this)))

  // TODO: positional $
  def update(f: this.type => Update): Lens = f(this).get(this)

  override def toString: String = "Subset("+longName+")"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Subset => super.equals(other) }
}