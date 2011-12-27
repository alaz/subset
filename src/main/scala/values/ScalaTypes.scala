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
package values

/** Getters and setters for complex Scala types, e.g. Traversable, Option, etc.
  */
trait ScalaTypesSerialization {
  import DBObjectLens._

  implicit def optionGetter[T](implicit r: ValueReader[T]) =
    new ValueReader[Option[T]] {
      override def unpack(o: Any): Option[Option[T]] = Some(r.unpack(o))
    }
  implicit def listGetter[T](implicit r: ValueReader[T]) = {
    import collection.JavaConversions._
    import org.bson.types.BasicBSONList

    ValueReader[List[T]]({
        case ar: Array[_] => ar flatMap {r.unpack _} toList
        case list: BasicBSONList => list flatMap {r.unpack _} toList
      })
  }
  // TODO: ValueReader[Map[String,T]]

  implicit def optionSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Option[T]] {
      override def pack(x: Option[T]): Option[Any] = x flatMap { w.pack _}
    }
  implicit def seqSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Traversable[T]] {
      override def pack(x: Traversable[T]): Option[Any] = Some( x flatMap {w.pack _} toArray )
    }
  implicit def tupleSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Tuple2[String,T]] {
      override def pack(x: Tuple2[String,T]): Option[Any] =
        w.pack(x._2) map {v => writer(x._1, v)(ValueWriter.defaultWriter[Any]).get}
    }
  // TODO: ValueWriter[Map[String,T]]
}
