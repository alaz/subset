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

import java.util.Date

/** Basic implicit getters and setters along with some explicit transformations
  *
  * Mostly unpacks primitives "as is", without attempt to convert from other type
  */
trait BaseSerialization {
  import java.util.regex.Pattern
  import util.matching.Regex
  import org.bson.types.{ObjectId, Symbol => BsonSymbol}
  import com.mongodb.DBObject

  implicit val symbolSetter = ValueWriter[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = ValueWriter[Regex](r => r.pattern)

  implicit val booleanGetter = ValueReader[Boolean]({ case b: Boolean => b })
  implicit val intGetter = ValueReader[Int]({ case i: Int => i })
  implicit val longGetter = ValueReader[Long]({ case l: Long => l })
  implicit val doubleGetter = ValueReader[Double]({ case d: Double => d })
  implicit val dateGetter = ValueReader[Date]({ case d: Date => d })

  implicit val dboGetter = ValueReader[DBObject]({ case dbo: DBObject => dbo })
  implicit val stringGetter = ValueReader[String]({
      case s: String => s
      case s: BsonSymbol => s.getSymbol
      case oid: ObjectId => oid.toString
    })
  implicit val symbolGetter = ValueReader[Symbol]({
      case s: Symbol => s
      case s: BsonSymbol => Symbol(s.getSymbol)
    })
  implicit val regexGetter = ValueReader[Regex]({
      case p: Pattern => new Regex(p.pattern)
      case r: Regex => r
    })
}
