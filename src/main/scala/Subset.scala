package com.osinka.subset

abstract class Subset(val name: String)(implicit outer: Path) extends Path {
  override val path: List[String] = outer.path :+ name
  implicit val myPath: Path = this

  def query(f: this.type => Serializer): Serializer = {
    f(this)
  }

  def update(f: this.type => Serializer): Serializer = {
    f(this)
  }

  // TODO: positional $
}