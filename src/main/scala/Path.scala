package com.osinka.subset

trait Path {
  def path: List[String]

  def longName = path mkString "."

  def relative(scope: Path) =
    if (path startsWith scope.path) Path(path.drop(scope.path.size))
    else this
}

object Path {
  private case class DefaultPath(override val path: List[String]) extends Path

  val empty: Path = DefaultPath(Nil)

  def apply(l: List[String]): Path = DefaultPath(l)
  def apply(n: String): Path = apply(n :: Nil)
}