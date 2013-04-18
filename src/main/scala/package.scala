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
package com.osinka

import com.mongodb.DBObject

/** == Getting Started ==
  * This 'package object' provides a number of implicits to ease the
  * use of the library. Thus the initial step is supposed to be
  * {{{
  * import com.osinka.subset._
  * }}}
  *
  * == Value conversions ==
  * '''Subset''' provides
  * a couple of type classes, `ValueReader[T]` and `ValueWriter[T]` to define
  * mechanisms of converting values to and from BSON values, and a library of
  * ''implicit''s for common Scala types.
  *
  * A developer may define own instances to support other types, e.g. if one needs
  * to store `BigDecimal` values in an integer BSON value
  * {{{
  * implicit val bigDecimalReader = ValueReader[BigDecimal]({
  *     case l: Long => BigDecimal(l, 2)
  *   })
  * implicit val bigDecimalWriter = ValueWriter[BigDecimal](bd => {
  *     assert(bd.scale == 2)
  *     (bd*100).setScale(0).toLong
  *   })
  * }}}
  *
  * There are few optional ''import''s:
  *  - `import JodaValues._` will make available conversions for Joda's `DateTime` values
  *  - `import SmartValues._` provides extended readers for primitive types and date, when
  *    readers try to extract a value even from incorrect BSON type (e.g. when an Int
  *    is stored in Double or in String)
  *
  * == Field ==
  * It is possible to pimp a string into a [[com.osinka.subset.Field]] with `"fieldName".fieldOf[T]`.
  *
  * Tuples made of `Field[T]` and an object of the same `T` can be implicitly converted into
  * `DBObject` or [[com.osinka.subset.Mutation]] if `T` has a [[com.osinka.subset.ValueWriter]].
  * {{{
  * val dbo: DBObject = "i".fieldOf[Int] -> 10
  * val mutation: Mutation = ("i".fieldOf[Int] -> 10) ~ ("s".fieldOf[String] -> "str")
  * }}}
  *
  * It's possible to use a string instead of field as well (but only to create a
  * `Mutation`):
  * {{{
  * val mutation: Mutation = ("i".fieldOf[Int] -> 10) ~ ("s" -> "str")
  * }}}
  *
  * == Subset ==
  * You may pimp a `String` into Subset with
  * {{{
  * val subset = "name".subset(Obj).of[T]
  * }}}
  *
  * where `Obj` is a container of fields and `T` is a subset's type
  *
  * == What's next? ==
  *  - Start from [[com.osinka.subset.Field]].
  *  - [[com.osinka.subset.Subset]] will give you a hint on how to work with subdocuments.
  *  - [[com.osinka.subset.query]] provides information on building queries.
  *  - [[com.osinka.subset.update]] is about "update modifiers".
  *  - [[com.osinka.subset.aggregation]] supports `aggregate`-"pipeline operators".
  *  - If you need details, [[com.osinka.subset.Mutation]],
  *    [[com.osinka.subset.ValueReader]] and [[com.osinka.subset.ValueWriter]] are the
  *    way to go.
  *
  * @see [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
package object subset {
  import query._

  // String to Field
  implicit def stringToField(name: String) = new FieldBlank(name)

  // Field conversions
  implicit def fieldTupleSerializer[T : ValueWriter](t: (Field[T], T)): Mutation = Mutation.writer(t._1.name, t._2)
  implicit def fieldTupleDBO[T : ValueWriter](t: (Field[T], T)): DBObject = fieldTupleSerializer[T](t).get

  // Mutations
  implicit def fToMutation(f: DBObject => DBObject): Mutation = Mutation.fToMutation(f)
  implicit def fToQMutation(f: Path => Mutation): QueryMutation = QueryMutation.fToQMutation(f)

  // Query
  type Query = query.Query
  val Query = query.Query

  // Update
  type Update = update.Update
  val Update = update.Update

  /** Convenience extractor
    *
    * It provides conjunction for use in pattern matching, e.g.
    *
    * {{{
    * val FieldI = "i".fieldOf[Int]
    * val FieldS = "s".fieldOf[String]
    *
    * dbo match {
    *   case FieldI(i) ~ FieldS(s) =>
    * }
    * }}}
    *
    * Based on the idea from [[http://stackoverflow.com/questions/2261358/pattern-matching-with-conjunctions-patterna-and-patternb Pattern Matching with Conjunctions (PatternA AND PatternB)]]
    */
  object ~ {
    def unapply[A](a: A) = Some((a,a))
  }
}
