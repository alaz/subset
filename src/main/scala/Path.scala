/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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