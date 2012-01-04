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
  * This 'package object' provides a number of implicits to ease conversions and
  * use of the library. Thus the initial step is supposed to be
  * {{{
  * import com.osinka.subset._
  * }}}
  * 
  * == Value conversions ==
  * '''Subset''' provides a means to convert Java/Scala types to/from BSON values.
  * You have several options:
  *  - if you are using Casbah, it already contains the conversion library
  *  - you may `import SmartValues._`. '''Subset''' will try to extract correct
  *    values from incorrect field contents (e.g. an integer value stored as a
  *    string field)
  *  - if you do not need this, do `import StrictValues._`
  *
  * == Field ==
  * It is possible to pimp a string into a [[com.osinka.subset.Field]] with `"fieldName".fieldOf[T]`.
  * 
  * Tuples made of `Field[T]` and an object of the same `T` can be implicitly converted into
  * `DBObject` or [[com.osinka.subset.DBObjectLens]] if `T` has a [[com.osinka.subset.ValueWriter]].
  * {{{
  * val dbo: DBObject = "i".fieldOf[Int] -> 10
  * val lens: DBObjectLens = ("i".fieldOf[Int] -> 10) ~ ("s".fieldOf[String] -> "str")
  * }}}
  * 
  * It's possible to use a string instead of field as well (but only to create a
  * `DBObjectLens`):
  * {{{
  * val lens: DBObjectLens = ("i".fieldOf[Int] -> 10) ~ ("s" -> "str")
  * }}}
  * 
  * == What's next? ==
  *  - Start from [[com.osinka.subset.Field]].
  *  - [[com.osinka.subset.Subset]] will give you a hint on how to work with subdocuments.
  *  - [[com.osinka.subset.query]] provides information on building queries.
  *  - [[com.osinka.subset.update]] is about "update modifiers".
  *  - If you need details, [[com.osinka.subset.DBObjectLens]], [[com.osinka.subset.Path]],
  *    [[com.osinka.subset.ValueReader]] and [[com.osinka.subset.ValueWriter]] are the
  *    way to go.
  *
  * @see [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
package object subset {
  import query._
  import update._

  // String to Field
  implicit def stringToField(name: String) =
    new {
      def fieldOf[T]: Field[T] = Field[T](name)
      def subset[Self](self: Self) =
        new {
          def of[T] = Subset[T,Self](name, self)
        }
    }

  // String Tuple
  implicit def stringTupleSerializer[T : ValueWriter](t: (String, T)): DBObjectLens = DBObjectLens.writer(t._1, t._2)

  // Field conversions
  implicit def fieldTupleSerializer[T : ValueWriter](t: (Field[T], T)): DBObjectLens = DBObjectLens.writer(t._1.name, t._2)
  implicit def fieldTupleDBO[T : ValueWriter](t: (Field[T], T)): DBObject = fieldTupleSerializer[T](t).get

  // DBObjectLenses
  implicit def lensToDBO(l: DBObjectLens): DBObject = l.get
  implicit def fToDBObjectLens(f: DBObject => DBObject): DBObjectLens = DBObjectLens.fToDBObjectLens(f)
  implicit def fToQDBObjectLens(f: Path => DBObjectLens): QueryLens = QueryLens.fToQDBObjectLens(f)

  // Query
  val Query = query.Query

  // Update
  val Update = update.Update

  // Explicit objects to import serialization strategy
  val StrictValues = values.StrictValues
  val SmartValues = values.SmartValues

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
