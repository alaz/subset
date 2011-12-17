package com.osinka.subset

import com.mongodb.DBObject

import Lens._
import QueryLens._

abstract class Subset(val name: String)(implicit outer: Path = Path.empty) extends Path {
  override val path: List[String] = outer.path :+ name
  implicit def scope: Path = this

  def apply(flist: Lens*): Lens = writer(name, flist reduceLeft {_ ~ _})

  def from(dbo: DBObject)(implicit r: ValueReader[DBObject]): Option[DBObject] = read[DBObject](name, dbo)

  /**
   * Creates a query as an $elemMatch relative to this document
   * 
   * http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch
   */
  def elemMatch(f: this.type => Query): Query = Query( relative(this, writer("$elemMatch", f(this).queryLens(this))) )

  /**
   * Creates an update operator positioned relative to this document
   * 
   * http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator
   */
  def updateMatch(f: this.type => Update): Lens = f(this).get(this)

  override def toString: String = "Subset "+longName

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Subset => super.equals(other) }
}