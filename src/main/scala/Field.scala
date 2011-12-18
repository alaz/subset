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
import query._
import update._
import Lens._

/** == A typed field ==
  * 
  * === Serialization ===
  * A field provides serialization/deserialization capabilities. 
  * 
  * === Tuples ===
  * It is possible to build Tuple serializers from several fields.
  * 
  * === Querying ===
  * A field is where query terms get created.
  * 
  * === Update operators ===
  * A field provides methods to create update operations.
  * 
  * === Convenience fields ===
  * There is a number of typical field types that can be of help. `Int` field (created by `int` method) is usually
  * used in MongoDB to declare sorting and indexes. `Any` field (created by `any` method) may be helpful to write
  * or read a field "as is". There is also a "positional" field (created by `first` method) that may be used
  * to update the first matched element in an array (in [[com.osinka.subset.update.Update]] operations),
  * see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]] for
  * details.
  * 
  * @see [[com.osinka.subset.Lens]]
  * @see [[com.osinka.subset.ValueReader]]
  * @see [[com.osinka.subset.ValueWriter]]
  */
class Field[T](val name: String)(implicit outer: Path = Path.empty) extends Path with FieldConditions[T] with Modifications[T] {
  override val path: List[String] = outer.path :+ name

  /** "Index" field
   * 
   * `Field[Int]` is of much help to produce `[Int]` queries (in `$special`, `$maxKey`, `$minKey`, sort, index, etc.)
   */
  def int: Field[Int] = new Field[Int](name)(outer)

  /** "Any" field
   * 
   * `Field[Any]` is of much help to insert custom objects or e.g. `org.bson.types.{MaxKey, MinKey}`
   */
  def any: Field[Any] = new Field[Any](name)(outer)

  /** "Positional" field
   *
   * Creates a positional field to update the first matched element in an array.
   * 
   * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]]
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
  /** Field factory method
    */
  def apply[T](name: String)(implicit outer: Path = Path.empty): Field[T] = new Field[T](name)
}