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

/** MongoDB field/subdocument path
  *
  * @see [[http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29 MongoDB dot notation]] and
  * [[http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29 Dot Notation (Reaching into Objects)]]
  */
trait Path {
  def path: List[String]

  def name: String = path.last

  /** Path in dot notation
    * @see [[http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29 MongoDB dot notation]]
    */
  def longName: String = path mkString "."

  def +(p: Path): Path = Path(path ::: p.path)

  def prefixString: String = "Path"
  override def toString: String = prefixString+" "+longName

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Path if prefixString == other.prefixString => path == other.path }

  override def hashCode: Int = path.hashCode
}

object Path {
  private class DefaultPath(override val path: List[String]) extends Path

  /** Empty [[com.osinka.subset.Path]]
    */
  val empty: Path = new DefaultPath(Nil)

  def apply(l: List[String]): Path = new DefaultPath(l)
  def apply(n: String): Path = apply(n :: Nil)
}