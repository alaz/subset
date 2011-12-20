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

  describe("Subset") {
    object Doc extends Subset[DBObject]("doc") {
      val f = "f".fieldOf[Int]

      object Sub extends Subset[DBObject]("sub") {
        val f = Field[Int]("f")
      }
    }

    it("has correct java methods") {
      Doc.toString must startWith("Subset")
      Doc.path must equal("doc" :: Nil)
      Doc must equal(Doc)

      Doc.Sub.path must equal("doc" :: "sub" :: Nil)
    }
    it("has correct longNames") {
      Doc.longName must equal("doc")
      Doc.Sub.longName must equal("doc.sub")
    }
    it("provides correct fields") {
      Doc.f.path must equal("doc" :: "f" :: Nil)
      Doc.Sub.f.path must equal("doc" :: "sub" :: "f" :: Nil)
    }
    it("serializes fields specified") {
      val dbo: DBObject = Doc(Doc.f -> 10, Doc.Sub(Doc.Sub.f -> 5))

      val doc = DBObjectLens.read[DBObject]("doc", dbo)
      doc must be('defined)
      doc.get must containKeyValue("f" -> 10)

      val sub = DBObjectLens.read[DBObject]("sub", doc.get)
      sub must be('defined)
      sub.get must containKeyValue("f" -> 5)
    }
    it("deserializes correctly") {
      val dbo = start.push("doc").append("f", 10).get
      Doc.unapply(dbo) must equal(Some(start("f", 10).get))
      Doc.Sub.unapply(dbo) must equal(None)
    }
  }
  describe("Typed Subset") {
    class Doc(val f: Int)

    object Doc extends Subset[Doc]("doc") {
      val f = "f".fieldOf[Int]
    }
    
    implicit val docReader = ValueReader[Doc]({
        case Doc.f(f) => new Doc(f)
      })

    it("has extractor") {
      val dbo = start.push("doc").append("f", 10).get
      dbo match {
        case Doc(doc) => doc.f must equal(10)
        case _ => fail("must match")
      }
    }
  }
  describe("Array of subsets") {
    it("works") { pending }
  }
}
