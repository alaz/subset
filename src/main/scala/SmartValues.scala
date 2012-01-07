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

import java.util.Date

/** Provides a smart deserialization of BSON values
  */
object SmartValues extends SmartValues

/** unpack primitives and try to convert from other type:
  * 
  * - tries to get Int, Long, Double, Byte from String
  * - tries to get DateTime (java.util.Date) from Int (as a number of seconds from the epoch)
  *   or Long (as a number of milliseconds from the epoch)
  */
trait SmartValues {
  import Extractors._
  import ValueReader._

  implicit val booleanRecoveringGetter = ValueReader[Boolean]({
      case b: Boolean => b
      case i: Int => i != 0
      case l: Long => l != 0
    })
  implicit val shortRecoveringGetter = ValueReader[Short]({
      case i: Int => i.shortValue
      case l: Long => l.shortValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i.shortValue })

  implicit val intRecoveringGetter = ValueReader[Int]({
      case i: Int => i
      case l: Long => l.intValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i })
  
  implicit val longRecoveringGetter = ValueReader[Long]({
      case i: Int => i.longValue
      case l: Long => l
    }) orElse stringGetter.andThen({ case AsLong(l) => l })
  
  implicit val byteRecoveringGetter = ValueReader[Byte]({
      case i: Int => i.byteValue
      case l: Long => l.byteValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i.byteValue })

  implicit val doubleRecoveringGetter = ValueReader[Double]({
      case i: Int => i.doubleValue
      case l: Long => l.doubleValue
      case d: Double => d
    }) orElse stringGetter.andThen({ case AsDouble(d) => d })
  implicit val floatRecoveringGetter = ValueReader[Float]({
      case i: Int => i.floatValue
      case l: Long => l.floatValue
      case d: Double => d.floatValue
    }) orElse stringGetter.andThen({ case AsDouble(d) => d.floatValue })

  implicit val dateRecoveringGetter = ValueReader[Date]({
      case d: Date => d
      case i: Int => new Date(i*1000L)
      case l: Long => new Date(l)
    })
}

