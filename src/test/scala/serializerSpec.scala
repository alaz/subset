package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class serializerSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Serializer") {
    // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
    def dbo = start("i", 10).push("inner").add("s", "string")/*.add("a", Array(1,2))*/.get
    def serializer = Serializer(_ => dbo)

    it("has `equals`") {
      serializer must equal(serializer)
      serializer.get must equal(dbo)
    }
    it("has `hashCode`") {
      serializer.hashCode must equal(serializer.hashCode)
      serializer.get.hashCode must equal(dbo.hashCode)
    }
    it("has `toString`") {
      serializer.toString must startWith("Serializer")
    }
  }
}
