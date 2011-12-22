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

object Document {
  val DocumentId = "_id".fieldOf[ObjectId]
  val Namespace = "_ns".fieldOf[String]

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