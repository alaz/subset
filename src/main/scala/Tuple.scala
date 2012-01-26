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
class Tuple2Subset[T1,T2](val f1: Field[T1], val f2: Field[T2]) { tuple =>
  type SelfTuple = (T1,T2)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
    */
  // TODO: A <% T : ValueWriter
  def apply(t2: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2]): Mutation =
    f1(t2._1) ~ f2(t2._2)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2]): Option[SelfTuple] =
    for {x1 <- f1.unapply(dbo); x2 <- f2.unapply(dbo)}
    yield (x1, x2)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  /** Concatenates this tuple with another field to create next arity
    */
  def ~[T3](f3: Field[T3]) = new Tuple3Subset[T1,T2,T3](this, f3)

  override def toString: String = "Tuple2Subset %s,%s".format(f1, f2)
}

/** A tuple made of 3 fields
 */
class Tuple3Subset[T1,T2,T3](val e2: Tuple2Subset[T1,T2], val f3: Field[T3]) {
  type SelfTuple = (T1,T2,T3)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t3: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3]): Mutation =
    e2.apply( (t3._1, t3._2) )(s1,s2) ~ f3(t3._3)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3]): Option[SelfTuple] =
    for {t2 <- e2.unapply(dbo); x3 <- f3.unapply(dbo)} yield (t2._1, t2._2, x3)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  /** Concatenates this tuple with another field to create next arity
   */
  def ~[T4](f4: Field[T4]) = new Tuple4Subset[T1,T2,T3,T4](this, f4)

  override def toString: String = "Tuple3Subset %s,%s,%s".format(e2.f1, e2.f2, f3)
}

/** A tuple made of 4 fields
 */
class Tuple4Subset[T1,T2,T3,T4](val e3: Tuple3Subset[T1,T2,T3], val f4: Field[T4]) {
  type SelfTuple = (T1,T2,T3,T4)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t4: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4]): Mutation =
    e3.apply( (t4._1, t4._2, t4._3) )(s1,s2,s3) ~ f4(t4._4)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4]): Option[SelfTuple] =
    for {t3 <- e3.unapply(dbo); x4 <- f4.unapply(dbo)} yield (t3._1, t3._2, t3._3, x4)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  /** Concatenates this tuple with another field to create next arity
   */
  def ~[T5](f5: Field[T5]) = new Tuple5Subset[T1,T2,T3,T4,T5](this, f5)

  override def toString: String = "Tuple4Subset %s,%s,%s,%s".format(e3.e2.f1, e3.e2.f2, e3.f3, f4)
}

/** A tuple made of 5 fields
 */
class Tuple5Subset[T1,T2,T3,T4,T5](val e4: Tuple4Subset[T1,T2,T3,T4], val f5: Field[T5]) {
  type SelfTuple = (T1,T2,T3,T4,T5)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t5: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5]): Mutation =
    e4.apply( (t5._1, t5._2, t5._3, t5._4) )(s1,s2,s3,s4) ~ f5(t5._5)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5]): Option[SelfTuple] =
    for {t4 <- e4.unapply(dbo); x5 <- f5.unapply(dbo)} yield (t4._1, t4._2, t4._3, t4._4, x5)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  def ~[T6](f6: Field[T6]) = new Tuple6Subset[T1,T2,T3,T4,T5,T6](this, f6)

  override def toString: String = "Tuple5Subset %s,%s,%s,%s,%s".format(e4.e3.e2.f1, e4.e3.e2.f2, e4.e3.f3, e4.f4, f5)
}

/** A tuple made of 6 fields
 */
class Tuple6Subset[T1,T2,T3,T4,T5,T6](val e5: Tuple5Subset[T1,T2,T3,T4,T5], val f6: Field[T6]) {
  type SelfTuple = (T1,T2,T3,T4,T5,T6)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t6: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6]): Mutation =
    e5.apply( (t6._1, t6._2, t6._3, t6._4, t6._5) )(s1,s2,s3,s4,s5) ~ f6(t6._6)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6]): Option[SelfTuple] =
    for {t5 <- e5.unapply(dbo); x6 <- f6.unapply(dbo)} yield (t5._1, t5._2, t5._3, t5._4, t5._5, x6)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  def ~[T7](f7: Field[T7]) = new Tuple7Subset[T1,T2,T3,T4,T5,T6,T7](this, f7)

  override def toString: String = "Tuple6Subset %s,%s,%s,%s,%s,%s".format(e5.e4.e3.e2.f1, e5.e4.e3.e2.f2, e5.e4.e3.f3, e5.e4.f4, e5.f5, f6)
}

/** A tuple made of 7 fields
 */
class Tuple7Subset[T1,T2,T3,T4,T5,T6,T7](val e6: Tuple6Subset[T1,T2,T3,T4,T5,T6], val f7: Field[T7]) {
  type SelfTuple = (T1,T2,T3,T4,T5,T6,T7)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t7: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7]): Mutation =
    e6.apply( (t7._1, t7._2, t7._3, t7._4, t7._5, t7._6) )(s1,s2,s3,s4,s5,s6) ~ f7(t7._7)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7]): Option[SelfTuple] =
    for {t6 <- e6.unapply(dbo); x7 <- f7.unapply(dbo)} yield (t6._1, t6._2, t6._3, t6._4, t6._5, t6._6, x7)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  def ~[T8](f8: Field[T8]) = new Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8](this, f8)

  override def toString: String = "Tuple7Subset %s,%s,%s,%s,%s,%s,%s".format(e6.e5.e4.e3.e2.f1, e6.e5.e4.e3.e2.f2, e6.e5.e4.e3.f3, e6.e5.e4.f4, e6.e5.f5, e6.f6, f7)
}

/** A tuple made of 8 fields
 */
class Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8](val e7: Tuple7Subset[T1,T2,T3,T4,T5,T6,T7], val f8: Field[T8]) {
  type SelfTuple = (T1,T2,T3,T4,T5,T6,T7,T8)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t8: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7], s8: ValueWriter[T8]): Mutation =
    e7.apply( (t8._1, t8._2, t8._3, t8._4, t8._5, t8._6, t8._7) )(s1,s2,s3,s4,s5,s6,s7) ~ f8(t8._8)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8]): Option[SelfTuple] =
    for {t7 <- e7.unapply(dbo); x8 <- f8.unapply(dbo)} yield (t7._1, t7._2, t7._3, t7._4, t7._5, t7._6, t7._7, x8)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

  def ~[T9](f9: Field[T9]) = new Tuple9Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9](this, f9)

  override def toString: String = "Tuple8Subset %s,%s,%s,%s,%s,%s,%s,%s".format(e7.e6.e5.e4.e3.e2.f1, e7.e6.e5.e4.e3.e2.f2, e7.e6.e5.e4.e3.f3, e7.e6.e5.e4.f4, e7.e6.e5.f5, e7.e6.f6, e7.f7, f8)
}

/** A tuple made of 9 fields
 */
class Tuple9Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9](val e8: Tuple8Subset[T1,T2,T3,T4,T5,T6,T7,T8], val f9: Field[T9]) {
  type SelfTuple = (T1,T2,T3,T4,T5,T6,T7,T8,T9)

  /** Serialize a tuple into a [[com.osinka.subset.Mutation]]
   */
  def apply(t9: SelfTuple)(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5], s6: ValueWriter[T6], s7: ValueWriter[T7], s8: ValueWriter[T8], s9: ValueWriter[T9]): Mutation =
    e8.apply( (t9._1, t9._2, t9._3, t9._4, t9._5, t9._6, t9._7, t9._8) )(s1,s2,s3,s4,s5,s6,s7,s8) ~ f9(t9._9)

  /** Deserialize a `DBObject` into a tuple
   */
  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8], g9: ValueReader[T9]): Option[SelfTuple] =
    for {t8 <- e8.unapply(dbo); x9 <- f9.unapply(dbo)} yield (t8._1, t8._2, t8._3, t8._4, t8._5, t8._6, t8._7, t8._8, x9)

  def prefix[A](pf: PartialFunction[(SelfTuple, DBObject), A])(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5], g6: ValueReader[T6], g7: ValueReader[T7], g8: ValueReader[T8], g9: ValueReader[T9]) =
    new PartialFunction[DBObject, A] {
      override def isDefinedAt(dbo: DBObject): Boolean = unapply(dbo) map {x => pf.isDefinedAt(x -> dbo)} getOrElse false
      override def apply(dbo: DBObject): A = pf.apply(unapply(dbo).get -> dbo)
    }

//  def ~[T10](f10: Field[T10]) = new Tuple10Subset[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](this, f10.name)

  override def toString: String = "Tuple9Subset %s,%s,%s,%s,%s,%s,%s,%s,%s".format(e8.e7.e6.e5.e4.e3.e2.f1, e8.e7.e6.e5.e4.e3.e2.f2, e8.e7.e6.e5.e4.e3.f3, e8.e7.e6.e5.e4.f4, e8.e7.e6.e5.f5, e8.e7.e6.f6, e8.e7.f7, e8.f8, f9)
}

// TODO: Tuple10..22
