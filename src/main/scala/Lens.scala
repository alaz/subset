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

import com.mongodb.{DBObject,BasicDBObjectBuilder}

/** The low level mechanism for modifying DBObjects.
  *
  * === Composition ===
  * Basically the lens is just a function taking one `DBObject` and returning another.
  *
  * ''Note'': Subset does not declare if a lens must return the same `DBObject` or another one.
  *
  * We can compose two `DBObjectLens` objects into a single `DBObjectLens` just like function composition.
  * You may use `andThen` (and you will get `(DBObject => DBObject)` as a result, or you may use method `~`
  * (and you'll get `DBObjectLens`).
  *
  * {{{
  * val dbo: DBObject =
  * val lens1 = DBObjectLens.writer[Int]("x", 10)
  * val lens2 = DBObjectLens.writer[String]("s", "str")
  *
  * val lens = lens1 ~ lens2
  * lens(dbo) must (containKeyValue("x" -> 10) and containKeyValue("s" -> "str"))
  * }}}
  *
  * === Producing new DBObject ===
  * It's very typical to apply a `DBObjectLens` to an empty `DBObject`, thus using `DBObjectLens` as a
  * `DBObject` generator. Method `get` does this explicitly, however ''subset'' provides an implicit
  * conversion from `DBObjectLens` to `DBObject` as well.
  *
  * === Modifying an DBObject ===
  *
  * Since `DBObjectLens` is a function `(DBObject => DBObject)`, you may apply it directly
  * to the DBObject in order to modify it. It also has a method `:~>` that does the same:
  * {{{
  * val resultingDBObject = field1(fValue) ~ anotherField(anotherValue) :~> existingDBObject
  * }}}
  *
  * The right-associative method is called `<~:`, so that you may write
  * {{{
  * val resultingDBObject = existingDBObject <~: field1(fValue)
  * }}}
  *
  * === Using DBObjectLens in subset ===
  * Most ''subset'' classes are subtypes of `DBObjectLens`, so that it becomes possible to
  * compose them and apply to already existing `DBObject` values.
  *
  * A `DBObjectLens` is a convenient way to stack modifications together and then modify an existing `DBObject`, thus
  * providing interoperability with existing code.
  */
trait DBObjectLens extends (DBObject => DBObject) {
  /** Applies this lens to an empty `DBObject`
    * @return `DBObject`
    */
  def get: DBObject = apply(BasicDBObjectBuilder.start.get)

  /** Compose two lenses.
    * @return a composition of lenses
    */
  def ~ (other: DBObjectLens): DBObjectLens = DBObjectLens(this andThen other)

  /** Apply this lens to the DBObject
    */
  def :~> (dbo: DBObject): DBObject = apply(dbo)

  /** Apply this lens to the DBObject (right-associative version)
   */
  def <~: (dbo: DBObject): DBObject = apply(dbo)

  def prefixString = "DBObjectLens"

  override def toString: String = prefixString+get

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: DBObjectLens if prefixString == other.prefixString => get == other.get }

  override def hashCode: Int = get.hashCode
}

/** Provides a factory method to create lenses from `DBObject => DBObject` functions and a couple
  * convenience methods.
  */
object DBObjectLens {
  // Factory object
  def apply(f: DBObject => DBObject): DBObjectLens =
    new DBObjectLens {
      def apply(dbo: DBObject): DBObject = f(dbo)
    }

  implicit def fToDBObjectLens(f: DBObject => DBObject): DBObjectLens = apply(f)
  implicit def lensWriter: ValueWriter[DBObjectLens] = ValueWriter[DBObjectLens](_.get)

  /** Reads a value from `DBObject` by key.
    *
    * This method makes use of [[com.osinka.subset.ValueReader]] implicit to unpack the object correctly.
    * 
    * @see [[com.osinka.subset.ValueReader]]
    */
  def read[T](key: String, dbo: DBObject)(implicit reader: ValueReader[T]): Option[T] =
    Option(dbo.get(key)) flatMap {reader.unpack(_)}

  /** Creates a lens that writes a typed key-value.
    * 
    * Makes use of [[com.osinka.subset.ValueWriter]] implicit.
    * 
    * @see [[com.osinka.subset.ValueWriter]]
    */
  def writer[T](key: String, x: T)(implicit w: ValueWriter[T]): DBObjectLens =
    (dbo: DBObject) => {
        w.pack(x) foreach {dbo.put(key, _)}
        dbo
      }
}

