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

@RunWith(classOf[JUnitRunner])
class subsetSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import StrictValues._

  describe("Untyped Subset") {
    object Sub {
      val f = Field[Int]("f")
    }
    object Doc {
      val f = "f".fieldOf[Int]
      val sub = "sub".subset(Sub).of[DBObject]
    }
    val doc = "doc".subset(Doc).of[DBObject]

    it("has correct java methods") {
      doc.toString must startWith("Subset")
      doc.path must equal("doc" :: Nil)
      doc must equal(doc)
    }
    it("has correct longNames") {
      doc.longName must equal("doc")
      Doc.sub.longName must equal("sub")
    }
    it("provides correct fields") {
      Doc.f.path must equal("f" :: Nil)
      Sub.f.path must equal("f" :: Nil)
    }
    it("serializes fields specified") {
      val dbo: DBObject = doc(Doc.f(10) ~ Doc.sub(Sub.f -> 5))

      val d = DBObjectLens.read[DBObject]("doc", dbo)
      d must be('defined)
      d.get must containKeyValue("f" -> 10)

      val s = DBObjectLens.read[DBObject]("sub", d.get)
      s must be('defined)
      s.get must containKeyValue("f" -> 5)
    }
    it("deserializes correctly") {
      val dbo = start.push("doc").append("f", 10).get
      doc.unapply(dbo) must equal(Some(start("f", 10).get))
      Doc.sub.unapply(dbo) must equal(None)
    }
  }
  describe("Typed Subset") {
    class Doc(val f: Int)

    object Doc {
      val f = "f".fieldOf[Int]
    }
    val doc = "doc".subset(Doc).of[Doc]

    implicit val docReader = ValueReader[Doc]({
      case Doc.f(f) => new Doc(f)
    })
    implicit val docWriter = ValueWriter[Doc](d => Doc.f(d.f).get)

    it("has serializer") {
      doc(new Doc(10)).get must equal(start.push("doc").append("f", 10).get)
    }
    it("has extractor") {
      val dbo = start.push("doc").append("f", 10).get
      dbo match {
        case doc(d) => d.f must equal(10)
        case _ => fail("must match")
      }
    }
  }
}
