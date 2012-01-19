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
import Mutation._

/** A tuple made of two fields
  */
class Tuple2Subset[T1,T2](val f1: String, val f2: String) { tuple =>
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
    */
  // TODO: A <% T : ValueWriter
  def apply(t2: (T1,T2))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2]): Mutation =
    writer(f1, t2._1) andThen writer(f2, t2._2)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2]): Option[(T1,T2)] =
    for {x1 <- read[T1](f1, dbo); x2 <- read[T2](f2, dbo)}
    yield (x1, x2)

  /** Concatenates this tuple with another field to create next arity
    */
  def ~[T3](f3: Field[T3]) = new Tuple3Subset[T1,T2,T3](this, f3.name)
}

/** A tuple made of 3 fields
 */
class Tuple3Subset[T1,T2,T3](val e2: Tuple2Subset[T1,T2], val f3: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t3: (T1,T2,T3))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3]): Mutation =
    e2.apply( (t3._1, t3._2) )(s1,s2) andThen writer(f3, t3._3)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3]): Option[(T1,T2,T3)] =
    for {t2 <- e2.unapply(dbo); x3 <- read[T3](f3, dbo)} yield (t2._1, t2._2, x3)

  /** Concatenates this tuple with another field to create next arity
   */
  def ~[T4](f4: Field[T4]) = new Tuple4Subset[T1,T2,T3,T4](this, f4.name)
}

/** A tuple made of 4 fields
 */
class Tuple4Subset[T1,T2,T3,T4](val e3: Tuple3Subset[T1,T2,T3], val f4: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t4: (T1,T2,T3,T4))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4]): Mutation =
    e3.apply( (t4._1, t4._2, t4._3) )(s1,s2,s3) andThen writer(f4, t4._4)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4]): Option[(T1,T2,T3,T4)] =
    for {t3 <- e3.unapply(dbo); x4 <- read[T4](f4, dbo)} yield (t3._1, t3._2, t3._3, x4)

  /** Concatenates this tuple with another field to create next arity
   */
  def ~[T5](f5: Field[T5]) = new Tuple5Subset[T1,T2,T3,T4,T5](this, f5.name)
}

/** A tuple made of 5 fields
 */
class Tuple5Subset[T1,T2,T3,T4,T5](val e4: Tuple4Subset[T1,T2,T3,T4], val f5: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t5: (T1,T2,T3,T4,T5))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5]): Mutation =
    e4.apply( (t5._1, t5._2, t5._3, t5._4) )(s1,s2,s3,s4) andThen writer(f5, t5._5)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5]): Option[(T1,T2,T3,T4,T5)] =
    for {t4 <- e4.unapply(dbo); x5 <- read[T5](f5, dbo)} yield (t4._1, t4._2, t4._3, t4._4, x5)

  def ~[T6](f6: Field[T6]) = new Tuple6Subset[T1,T2,T3,T4,T5,T6](this, f6.name)
}

/** A tuple made of 6 fields
 */
class Tuple6Subset[T1,T2,T3,T4,T5,T6](val e5: Tuple5Subset[T1,T2,T3,T4,T5], val f6: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t6: (T1,T2,T3,T4,T5,T6))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6]): Mutation =
    e5.apply( (t6._1, t6._2, t6._3, t6._4, t6._5) )(s1,s2,s3,s4,s5) andThen writer(f6, t6._6)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6]): Option[(T1,T2,T3,T4,T5,T6)] =
    for {t5 <- e5.unapply(dbo); x6 <- read[T6](f6, dbo)} yield (t5._1, t5._2, t5._3, t5._4, t5._5, x6)

  def ~[T7](f7: Field[T7]) = new Tuple7Subset[T1,T2,T3,T4,T5,T6,T7](this, f7.name)
}

/** A tuple made of 7 fields
 */
class Tuple7Subset[T1,T2,T3,T4,T5,T6,T7](val e6: Tuple6Subset[T1,T2,T3,T4,T5,T6], val f7: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t7: (T1,T2,T3,T4,T5,T6,T7))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7]): Mutation =
    e6.apply( (t7._1, t7._2, t7._3, t7._4, t7._5, t7._6) )(s1,s2,s3,s4,s5,s6) andThen writer(f7, t7._7)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7]): Option[(T1,T2,T3,T4,T5,T6,T7)] =
    for {t6 <- e6.unapply(dbo); x7 <- read[T7](f7, dbo)} yield (t6._1, t6._2, t6._3, t6._4, t6._5, t6._6, x7)

  def ~[T8](f8: Field[T8]) = new Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8](this, f8.name)
}

/** A tuple made of 8 fields
 */
class Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8](val e7: Tuple7Subset[T1,T2,T3,T4,T5,T6,T7], val f8: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t8: (T1,T2,T3,T4,T5,T6,T7,T8))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7], s8: ValueWriter[T8]): Mutation =
    e7.apply( (t8._1, t8._2, t8._3, t8._4, t8._5, t8._6, t8._7) )(s1,s2,s3,s4,s5,s6,s7) andThen writer(f8, t8._8)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8]): Option[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    for {t7 <- e7.unapply(dbo); x8 <- read[T8](f8, dbo)} yield (t7._1, t7._2, t7._3, t7._4, t7._5, t7._6, t7._7, x8)

  def ~[T9](f9: Field[T9]) = new Tuple9Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9](this, f9.name)
}

/** A tuple made of 9 fields
 */
class Tuple9Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9](val e8: Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8], val f9: String) {
  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t9: (T1,T2,T3,T4,T5,T6,T7,T8,T9))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7], s8: ValueWriter[T8], s9: ValueWriter[T9]): Mutation =
    e8.apply( (t9._1, t9._2, t9._3, t9._4, t9._5, t9._6, t9._7, t9._8) )(s1,s2,s3,s4,s5,s6,s7,s8) andThen writer(f9, t9._9)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8], g9: ValueReader[T9]): Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] =
    for {t8 <- e8.unapply(dbo); x9 <- read[T9](f9, dbo)} yield (t8._1, t8._2, t8._3, t8._4, t8._5, t8._6, t8._7, t8._8, x9)

//  def ~[T10](f10: Field[T10]) = new Tuple10Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](this, f10.name)
}

// TODO: Tuple10..22
