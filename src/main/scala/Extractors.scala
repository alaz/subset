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

import scala.util.control.Exception._
import org.bson.types.ObjectId

/** Few generic extractors.
  */
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
