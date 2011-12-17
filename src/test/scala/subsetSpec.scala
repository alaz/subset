package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class subsetSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
  import Values._

  describe("Subset") {
    object Doc extends Subset("doc") {
      val f = "f".fieldOf[Int]

      object Sub extends Subset("sub") {
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

      val doc = Lens.read[DBObject]("doc", dbo)
      doc must be('defined)
      doc.get must containKeyValue("f" -> 10)

      val sub = Lens.read[DBObject]("sub", doc.get)
      sub must be('defined)
      sub.get must containKeyValue("f" -> 5)
    }
    it("deserializes correctly") {
      val dbo = start.push("doc").append("f", 10).get
      Doc.from(dbo) must equal(Some(start("f", 10).get))
      Doc.Sub.from(dbo) must equal(None)
    }
  }
  describe("Array of subsets") {
    it("works") { pending }
  }
}
