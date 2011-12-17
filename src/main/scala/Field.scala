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

import com.mongodb.DBObject
import Lens._

class Field[T](val name: String)(implicit outer: Path = Path.empty) extends Path with FieldConditions[T] with Modifications[T] {
  override val path: List[String] = outer.path :+ name

  /**
   * Get "Index" field
   * 
   * Field[Int] is of much help to produce [Int] queries (in $special, $maxKey, $minKey, sort, index, etc.)
   */
  def int: Field[Int] = new Field[Int](name)(outer)

  /**
   * Get "Any" field
   * 
   * Field[Any] is of much help to insert custom objects or e.g. org.bson.types.{MaxKey, MinKey}
   */
  def any: Field[Any] = new Field[Any](name)(outer)

  /**
   * Creates a positional field to update the first matched element in an array
   * 
   * http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator
   */
  def first: Field[T] = new Field[T]("$")(Path(path))

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

  def apply(x: T)(implicit setter: ValueWriter[T]): Lens = writer(name, x)

  def unapply(dbo: DBObject)(implicit getter: ValueReader[T]): Option[T] = read[T](name, dbo)

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Field[_] => super.equals(other) }

  override def toString: String = "Field "+longName
}

object Field {
  def apply[T](name: String)(implicit outer: Path = Path.empty): Field[T] = new Field[T](name)
}