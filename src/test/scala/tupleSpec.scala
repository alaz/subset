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

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.start

import Implicits._
import SmartValues._

@RunWith(classOf[JUnitRunner])
class tupleSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Tuple deserializer") {
    it("deserializes Tuple2") {
      val T2 = "i".fieldOf[Int] ~ "s".fieldOf[String]
      T2.unapply(start.get) must equal(None)
      T2.unapply(start("i", 10).get) must equal(None)
      T2.unapply(start("i", 10).add("s", "str").get) must equal(Some(10 -> "str"))
      T2.unapply(start("i", "10").add("s", "str").get) must equal(Some(10 -> "str"))
    }
    it("deserializes Tuple3") {
      val T3 = "i".fieldOf[Int] ~ "s".fieldOf[String] ~ "d".fieldOf[Double]
      T3.unapply(start.get) must equal(None)
      T3.unapply(start("i", 10).get) must equal(None)
      T3.unapply(start("i", 10).add("s", "str").get) must equal(None)
      T3.unapply(start("i", 10).add("s", "str").add("d", 1.67).get) must equal(Some( (10, "str", 1.67) ))
      T3.unapply(start("i", "10").add("s", "str").add("d", "1.67").get) must equal(Some( (10, "str", 1.67) ))

      // or invoke extractor
      start("i", "10").add("s", "str").add("d", "1.67").get match {
        case T3(i, s, d) =>
          i must equal(10)
          s must equal("str")
          d must equal(1.67)
        case _ =>
          fail("T3 extractor failed")
      }
    }
  }

  describe("Tuple serializer") {
    it("serializes Tuple2") {
      val T2 = "i".fieldOf[Int] ~ "s".fieldOf[String]
      val dbo: DBObject = T2(10 -> "str")
      dbo must (containKeyValue("i" -> new java.lang.Integer(10)) and containKeyValue("s" -> "str"))
    }
    it("serializes Tuple3") {
      val T3 = "i".fieldOf[Int] ~ "s".fieldOf[String] ~ "d".fieldOf[Double]
      val dbo: DBObject = T3(10, "str", 1.67)
      dbo must (containKeyValue("i" -> new java.lang.Integer(10)) and
                containKeyValue("s" -> "str") and
                containKeyValue("d" -> new java.lang.Double(1.67)))
    }
    it("serializes in sequence") {
      val T2 = "i".fieldOf[Int] ~ "s".fieldOf[String]
      val F1 = "d".fieldOf[Double]
      val dbo: DBObject = T2(10, "str") ~ F1(1.67)
      dbo must (containKeyValue("i" -> new java.lang.Integer(10)) and
                containKeyValue("s" -> "str") and
                containKeyValue("d" -> new java.lang.Double(1.67)))
    }
  }
}
