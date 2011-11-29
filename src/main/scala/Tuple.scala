package com.osinka.subset

import com.mongodb.DBObject

class Tuple2Extractor[T1,T2](val f1: String, val f2: String) {
  def unapply(dbo: DBObject)(implicit g1: Getter[T1], g2: Getter[T2]): Option[(T1,T2)] =
    for {x1 <- g1.get(f1, dbo); x2 <- g2.get(f2, dbo)} yield (x1, x2)

  def ~[T3](f3: Field[T3]) = new Tuple3Extractor[T1,T2,T3](this, f3.name)
}

class Tuple3Extractor[T1,T2,T3](val e2: Tuple2Extractor[T1,T2], val f3: String) {
  def unapply(dbo: DBObject)(implicit g1: Getter[T1], g2: Getter[T2], g3: Getter[T3]): Option[(T1,T2,T3)] =
    for {t2 <- e2.unapply(dbo); x3 <- g3.get(f3, dbo)} yield (t2._1, t2._2, x3)

  def ~[T4](f4: Field[T4]) = new Tuple4Extractor[T1,T2,T3,T4](this, f4.name)
}

class Tuple4Extractor[T1,T2,T3,T4](val e3: Tuple3Extractor[T1,T2,T3], val f4: String) {
  def unapply(dbo: DBObject)(implicit g1: Getter[T1], g2: Getter[T2], g3: Getter[T3], g4: Getter[T4]): Option[(T1,T2,T3,T4)] =
    for {t3 <- e3.unapply(dbo); x4 <- g4.get(f4, dbo)} yield (t3._1, t3._2, t3._3, x4)
  
  def ~[T5](f5: Field[T5]) = new Tuple5Extractor[T1,T2,T3,T4,T5](this, f5.name)
}

class Tuple5Extractor[T1,T2,T3,T4,T5](val e4: Tuple4Extractor[T1,T2,T3,T4], val f5: String) {
  def unapply(dbo: DBObject)(implicit g1: Getter[T1], g2: Getter[T2], g3: Getter[T3], g4: Getter[T4], g5: Getter[T5]): Option[(T1,T2,T3,T4,T5)] =
    for {t4 <- e4.unapply(dbo); x5 <- g5.get(f5, dbo)} yield (t4._1, t4._2, t4._3, t4._4, x5)
  
//  def ~[T5](f5: Field[T5]) = new TupleExtractor5[T1,T2,T3,T4,T5](this, f5.name)
}
