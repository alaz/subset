package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class subsetSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Subset") {
    it("has correct longNames") { pending }
    it("provides correct fields") { pending }
    it("serializes fields specified") { pending }
    it("deserializes correctly") { pending }
  }
  describe("Array of subsets") {
    it("works") { pending }
  }
}
