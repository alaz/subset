package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.util.Date
import org.bson.types.{Symbol => BsonSymbol}
import com.mongodb.BasicDBObjectBuilder

@RunWith(classOf[JUnitRunner])
class serializerSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Base primitives serializer") {
    val explicit = new BasePrimitivesSerializer {}

    it("must set") {
      applySetter("f", "val")(explicit.defaultSetter[String]) must containKeyValue("f" -> "val")
      applySetter("f", 10) must containKeyValue("f" -> new java.lang.Integer(10))
      applySetter("f", 'Sym) must containKeyValue("f" -> new BsonSymbol("Sym"))
    }
    it("must get") {
      val dbo = BasicDBObjectBuilder.start("s", "string").add("sym", new BsonSymbol("Sym")).get
      applyGetter[String]("s", dbo) must equal(Some("string"))
      applyGetter[Symbol]("s", dbo) must equal(None)
      applyGetter[Symbol]("sym", dbo) must equal(Some('Sym))
      applyGetter[String]("sym", dbo) must equal(None)
    }
  }
  describe("Recovering primitives serializer") {
    val explicit = new RecoveringPrimitivesSerializer {}

    it("recovers Int") {
      val dbo = BasicDBObjectBuilder.start("string", "10").add("int", 11).add("long", 109L).add("fail", "x13").get
      applyGetter[Int]("int", dbo)(explicit.intGetter) must equal(Some(11))
      applyGetter[Int]("string", dbo)(explicit.intGetter) must equal(Some(10))
      applyGetter[Int]("long", dbo)(explicit.intGetter) must equal(Some(109))
      applyGetter[Int]("fail", dbo)(explicit.intGetter) must equal(None)
    }
    it("recovers Long") {
      val dbo = BasicDBObjectBuilder.start("string", "10").add("int", 11).add("long", 109L).add("fail", "x13").get
      applyGetter[Long]("int", dbo)(explicit.longGetter) must equal(Some(11L))
      applyGetter[Long]("string", dbo)(explicit.longGetter) must equal(Some(10L))
      applyGetter[Long]("long", dbo)(explicit.longGetter) must equal(Some(109L))
      applyGetter[Long]("fail", dbo)(explicit.longGetter) must equal(None)
    }
    it("recovers Double") {
      val dbo = BasicDBObjectBuilder.start("string", "10.87").add("double", 11.76).add("long", 109L).add("int", 67).add("fail", "x13").get
      applyGetter[Double]("double", dbo)(explicit.doubleGetter) must equal(Some(11.76))
      applyGetter[Double]("int", dbo)(explicit.doubleGetter) must equal(Some(67.0))
      applyGetter[Double]("string", dbo)(explicit.doubleGetter) must equal(Some(10.87))
      applyGetter[Double]("long", dbo)(explicit.doubleGetter) must equal(Some(109.0))
      applyGetter[Double]("fail", dbo)(explicit.doubleGetter) must equal(None)
    }
    it("recovers Float") {
      val dbo = BasicDBObjectBuilder.start("string", "10.87").add("float", 11.76F).add("long", 109L).add("int", 67).add("fail", "x13").get
      applyGetter[Float]("float", dbo)(explicit.floatGetter) must equal(Some(11.76F))
      applyGetter[Float]("int", dbo)(explicit.floatGetter) must equal(Some(67.0F))
      applyGetter[Float]("string", dbo)(explicit.floatGetter) must equal(Some(10.87F))
      applyGetter[Float]("long", dbo)(explicit.floatGetter) must equal(Some(109.0F))
      applyGetter[Float]("fail", dbo)(explicit.floatGetter) must equal(None)
    }
    it("recovers Date") {
      // round to seconds
      val now = {
        val d = new Date
        d.setTime(d.getTime/1000 * 1000L)
        d
      }
      val dbo = BasicDBObjectBuilder.start("date", now).add("long", now.getTime).add("int", (now.getTime/1000).intValue).get
      applyGetter[Date]("date", dbo)(explicit.dateGetter) must equal(Some(now))
      applyGetter[Date]("int", dbo)(explicit.dateGetter) must equal(Some(now))
      applyGetter[Date]("long", dbo)(explicit.dateGetter) must equal(Some(now))
    }
  }
}
