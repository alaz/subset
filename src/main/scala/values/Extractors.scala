package com.osinka.subset
package values

import scala.util.control.Exception._
import org.bson.types.ObjectId

object Extractors {
  /** An extractor you may use to check and build `ObjectId` from a String
    */
  object AsObjectId {
    def unapply(s: String): Option[ObjectId] =
      catching(classOf[IllegalArgumentException]) opt { new ObjectId(s) }
  }

  object AsInt {
    def unapply(s: String): Option[Int] =
      catching(classOf[NumberFormatException]) opt { java.lang.Integer.valueOf(s).intValue }
  }  

  object AsLong {
    def unapply(s: String): Option[Long] =
      catching(classOf[NumberFormatException]) opt { java.lang.Long.valueOf(s).longValue }
  }  

  object AsDouble {
    def unapply(s: String): Option[Double] =
      catching(classOf[NumberFormatException]) opt { java.lang.Double.valueOf(s).doubleValue }
  }  
}
