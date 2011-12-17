package com.osinka.subset

trait Path {
  def path: List[String]

  def longName = path mkString "."

  def relativeTo(scope: Path) =
    if (path startsWith scope.path) Path(path.drop(scope.path.size))
    else this

  def positionIn(scope: Path) =
    if (scope.path.isEmpty) this
    else if (path startsWith scope.path) Path(scope.path ::: "$" :: path.drop(scope.path.size))
    else this

  override def toString: String = "Path "+longName

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Path => path == other.path }

  override def hashCode: Int = path.hashCode
}

object Path {
  private class DefaultPath(override val path: List[String]) extends Path

  val empty: Path = new DefaultPath(Nil)

  def apply(l: List[String]): Path = new DefaultPath(l)
  def apply(n: String): Path = apply(n :: Nil)
}