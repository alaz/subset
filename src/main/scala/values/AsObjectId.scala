package com.osinka.subset
package values

import scala.util.control.Exception._
import org.bson.types.ObjectId

/** An extractor you may use to check and build `ObjectId` from a String
  */
object AsObjectId {
  def unapply(s: String): Option[ObjectId] =
    catching(classOf[IllegalArgumentException]) opt { new ObjectId(s) }
}
