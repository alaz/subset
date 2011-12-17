package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class lensSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Lens") {
    // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
    def dbo = start("i", 10).push("inner").add("s", "string")/*.add("a", Array(1,2))*/.get
    def lens = Lens(_ => dbo)

    it("has `equals`") {
      lens must equal(lens)
      lens.get must equal(dbo)
    }
    it("has `hashCode`") {
      lens.hashCode must equal(lens.hashCode)
      lens.get.hashCode must equal(dbo.hashCode)
    }
    it("has `toString`") {
      lens.toString must startWith("Lens")
    }
    it("initialized from empty DBObject") {
      Lens(identity).get must be('empty)
    }
    it("has ValueWriter") {
      val w = implicitly[ValueWriter[Lens]]
    }
    it("allows conjunction") {
      val conj = lens ~ Lens(identity)
      conj.toString must startWith("Lens")
      conj must equal(lens)
    }
  }
}
