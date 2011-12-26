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

import org.bson.types.ObjectId
import com.mongodb.{DBObject,WriteResult}

/** A set of helper fields and methods to work with MongoDB documents
  */
object Document {
  /**[[org.bson.types.ObjectId]] field named "_id"
    *
    * useful to modify or extract document IDs.
    */
  val DocumentId = "_id".fieldOf[ObjectId]

  /**Namespace is a string and usually should be interpreted as a collection name.
    */
  val Namespace = "_ns".fieldOf[String]

  /** Make a modification and return an object's ID on success or `WriteResult` on error.
    *
    * This wrapper method is supposed to be used around `insert` in order to return
    * the new object's ID.
    */
  def newDoc[T](obj: T)(op: Any => WriteResult)(implicit reader: ValueReader[ObjectId], t2dbo: T => DBObject): Either[WriteResult, ObjectId] = {
    val dbo: DBObject = t2dbo(obj)
    val wr = op(dbo)

    if (wr.getError == null)
      dbo match {
        case DocumentId(id) => Right(id)
        case _ => Left(wr)
      }
    else
      Left(wr)
  }
}
