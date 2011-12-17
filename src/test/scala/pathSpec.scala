package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class pathSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Path") {
    it("must have an empty value") {
      Path.empty.path must be('empty)
    }
    it("may be created out of a String") {
      val p = Path("field")
      p.toString must startWith("Path")
      p must equal(Path("field"))
      p must equal(Path("field" :: Nil))
      p.hashCode must equal(Path("field").hashCode)
      p.path must equal(List("field"))
      p.longName must equal("field")
    }
    it("may be created out of a List[String]") {
      val p = Path("doc" :: "field" :: Nil)
      p.toString must startWith("Path")
      p.longName must equal("doc.field")
      p.path must equal("doc" :: "field" :: Nil)
    }
    it("calculates relative path") {
      val p = Path("doc" :: "field" :: Nil)
      p.relativeTo(Path.empty) must equal(p)
      p.relativeTo(Path("other")) must equal(p)
      p.relativeTo(Path("doc")) must equal(Path("field" :: Nil))
      p.relativeTo(Path("doc")) must equal(Path("field" :: Nil))
      p.relativeTo(Path("doc" :: "other" :: Nil)) must equal(p)
    }
    it("makes positional path") {
      val p = Path("doc" :: "field" :: Nil)
      p.positionIn(Path.empty) must equal(p)
      p.positionIn(Path("other")) must equal(p)
      p.positionIn(Path("doc" :: Nil)) must equal(Path("doc" :: "$" :: "field" :: Nil))
      p.positionIn(Path("doc" :: "other" :: Nil)) must equal(p)
    }
  }
}
