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
  * Basically a mutation is a function taking one `DBObject` and returning another.
  *
  * We can compose two `Mutation` objects into a single `Mutation` just like function composition.
  * You may use `andThen` (and you will get `(DBObject => DBObject)` as a result), or you may use method `~`
  * (and you'll get `Mutation`).
  *
  * {{{
  * val dbo: DBObject =
  * val mutation1 = Mutation.writer[Int]("x", 10)
  * val mutation2 = Mutation.writer[String]("s", "str")
  *
  * val mutation = mutation1 ~ mutation2
  * mutation(dbo) must (containKeyValue("x" -> 10) and containKeyValue("s" -> "str"))
  * }}}
  *
  * === Producing new DBObject ===
  * It's very typical to apply a `Mutation` to an empty `DBObject`, thus using `Mutation` as a
  * `DBObject` generator. Method `get` does this explicitly, however ''subset'' provides an implicit
  * conversion from `Mutation` to `DBObject` as well.
  *
  * === Modifying an DBObject ===
  *
  * Since `Mutation` is a function `(DBObject => DBObject)`, you may apply it directly
  * to the DBObject in order to modify it. It also has a method `:~>` that does the same:
  * {{{
  * val resultingDBObject = field1(fValue) ~ anotherField(anotherValue) :~> existingDBObject
  * // is the same as
  * val resultingDBObject = (field1(fValue) ~ anotherField(anotherValue))(existingDBObject)
  * }}}
  *
  * The right-associative method is called `<~:`, so that you may write
  * {{{
  * val resultingDBObject = existingDBObject <~: field1(fValue)
  * // is the same as
  * val resultingDBObject = field1(fValue)(existingDBObject)
  * }}}
  *
  * === Using Mutation in subset ===
  * Most ''subset'' classes are subtypes of `Mutation`, so that it becomes possible to
  * compose them and apply to already existing `DBObject` values.
  *
  * A `Mutation` is a convenient way to stack modifications together and then modify an existing `DBObject`, thus
  * providing interoperability with existing code.
  *
  * ''Note'': Subset does not declare if a mutation must return the same `DBObject` or another one.
  */
trait Mutation extends (DBObject => DBObject) {
  /** Applies this mutation to an empty `DBObject`
    * @return `DBObject`
    */
  def get: DBObject = apply(BasicDBObjectBuilder.start.get)

  /** Compose two mutations.
    * @return a composition of mutations
    */
  def ~ (other: Mutation): Mutation = Mutation(this andThen other)

  /** Apply this mutation to the DBObject
    */
  def :~> (dbo: DBObject): DBObject = apply(dbo)

  /** Apply this mutation to the DBObject (right-associative version)
   */
  def <~: (dbo: DBObject): DBObject = apply(dbo)

  def prefixString = "Mutation"

  override def toString: String = prefixString+get

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Mutation if prefixString == other.prefixString => get == other.get }

  override def hashCode: Int = get.hashCode
}

/** Provides a factory method to create mutations from `DBObject => DBObject` functions and a couple
  * convenience methods.
  */
object Mutation {
  /** @return A mutation that removes all the contents
    */
  def empty: Mutation = const(BasicDBObjectBuilder.start.get)

  /** @return A mutation that replaces the contents with the `DBObject` specified
    */
  def const(dbo: DBObject): Mutation = apply {_: DBObject => dbo}

  /** @return A mutation that does nothing, simply returns the same `DBObject` back
    */
  def noop: Mutation = apply(identity _)

  // Factory object
  def apply(f: DBObject => DBObject): Mutation =
    new Mutation {
      def apply(dbo: DBObject): DBObject = f(dbo)
    }

  implicit def fToMutation(f: DBObject => DBObject): Mutation = apply(f)
  implicit def mutationToDBO(l: Mutation): DBObject = l.get
  implicit def mutationWriter: ValueWriter[Mutation] = ValueWriter[Mutation](mutationToDBO _)

  /** Reads a value from `DBObject` by key.
    *
    * This method makes use of [[com.osinka.subset.ValueReader]] implicit to unpack the object correctly.
    *
    * @see [[com.osinka.subset.ValueReader]]
    */
  def read[T](key: String, dbo: DBObject)(implicit reader: ValueReader[T]): Option[T] =
    Option(dbo.get(key)) flatMap {reader.unpack(_)}

  /** Creates a mutation that writes a typed key-value.
    *
    * Makes use of [[com.osinka.subset.ValueWriter]] implicit.
    *
    * @return a mutation writing the key-value specified
    * @see [[com.osinka.subset.ValueWriter]]
    */
  def writer[T](key: String, x: T)(implicit w: ValueWriter[T]): Mutation =
    (dbo: DBObject) => {
        w.pack(x) foreach {dbo.put(key, _)}
        dbo
      }

  def modifier[T,R](key: String, f: T => R)(implicit r: ValueReader[T], w: ValueWriter[R]): Mutation =
    (dbo: DBObject) =>
      read[T](key, dbo) map {t => writer[R](key, f(t)) :~> dbo} getOrElse dbo

  /** A mutation that removes a key
    */
  def remover(key: String): Mutation =
    (dbo: DBObject) => {
        dbo.removeField(key)
        dbo
      }
}

