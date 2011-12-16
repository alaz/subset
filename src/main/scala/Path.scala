package com.osinka.subset

trait Path {
  def path: List[String]

  def longName = path mkString "."

  def relative(scope: Path) =
    if (path startsWith scope.path) Path(path.drop(scope.path.size))
    else this

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Path => path == other.path }

  override def hashCode: Int = path.hashCode
}

object Path {
  private case class DefaultPath(override val path: List[String]) extends Path

  val empty: Path = DefaultPath(Nil)

  def apply(l: List[String]): Path = DefaultPath(l)
  def apply(n: String): Path = apply(n :: Nil)
}