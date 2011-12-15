package com.osinka.subset

trait Path {
  def path: List[String]

  def longName = path mkString "."
}

object Path {
  private case class DefaultPath(override val path: List[String]) extends Path

  val empty: Path = DefaultPath(Nil)

  def apply(n: String): Path = DefaultPath(n :: Nil)
}