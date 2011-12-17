package com.osinka.subset

import com.mongodb.DBObject
import Lens._

class Tuple2Subset[T1,T2](val f1: String, val f2: String) { tuple =>
  /*
   * Experimental "value (de)serializer" for Tuple2.
   * 
   * No go, because it's very confusing and requires creating manual "implicits"
  def serializer(implicit s1: ValueWriter[T1], s2: ValueWriter[T2]) =
    new Serializer[(T1,T2)] {
      override def apply(t2: (T1,T2)): DBObject => DBObject = tuple.apply(t2)(s1,s2)
    }

  def deserializer(implicit g1: ValueReader[T1], g2: ValueReader[T2]) =
    new Deserializer[(T1,T2)] {
      override def unapply(dbo: DBObject): Option[(T1,T2)] = tuple.unapply(dbo)(g1,g2)
    }
  */
  def apply(t2: (T1,T2))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2]): Lens =
    writer(f1, t2._1) andThen writer(f2, t2._2)

  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2]): Option[(T1,T2)] =
    for {x1 <- read[T1](f1, dbo); x2 <- read[T2](f2, dbo)}
    yield (x1, x2)

  def ~[T3](f3: Field[T3]) = new Tuple3Subset[T1,T2,T3](this, f3.name)
}

class Tuple3Subset[T1,T2,T3](val e2: Tuple2Subset[T1,T2], val f3: String) {
  def apply(t3: (T1,T2,T3))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3]): Lens =
    e2.apply( (t3._1, t3._2) )(s1,s2) andThen writer(f3, t3._3)

  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3]): Option[(T1,T2,T3)] =
    for {t2 <- e2.unapply(dbo); x3 <- read[T3](f3, dbo)} yield (t2._1, t2._2, x3)

  def ~[T4](f4: Field[T4]) = new Tuple4Subset[T1,T2,T3,T4](this, f4.name)
}

class Tuple4Subset[T1,T2,T3,T4](val e3: Tuple3Subset[T1,T2,T3], val f4: String) {
  def apply(t4: (T1,T2,T3,T4))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4]): Lens =
    e3.apply( (t4._1, t4._2, t4._3) )(s1,s2,s3) andThen writer(f4, t4._4)

  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4]): Option[(T1,T2,T3,T4)] =
    for {t3 <- e3.unapply(dbo); x4 <- read[T4](f4, dbo)} yield (t3._1, t3._2, t3._3, x4)
  
  def ~[T5](f5: Field[T5]) = new Tuple5Subset[T1,T2,T3,T4,T5](this, f5.name)
}

class Tuple5Subset[T1,T2,T3,T4,T5](val e4: Tuple4Subset[T1,T2,T3,T4], val f5: String) {
  def apply(t5: (T1,T2,T3,T4,T5))(implicit s1: ValueWriter[T1], s2: ValueWriter[T2], s3: ValueWriter[T3], s4: ValueWriter[T4], s5: ValueWriter[T5]): Lens =
    e4.apply( (t5._1, t5._2, t5._3, t5._4) )(s1,s2,s3,s4) andThen writer(f5, t5._5)

  def unapply(dbo: DBObject)(implicit g1: ValueReader[T1], g2: ValueReader[T2], g3: ValueReader[T3], g4: ValueReader[T4], g5: ValueReader[T5]): Option[(T1,T2,T3,T4,T5)] =
    for {t4 <- e4.unapply(dbo); x5 <- read[T5](f5, dbo)} yield (t4._1, t4._2, t4._3, t4._4, x5)
  
//  def ~[T5](f5: Subset[T5]) = new TupleSubset5[T1,T2,T3,T4,T5](this, f5.name)
}

// TODO: Tuple6..22
