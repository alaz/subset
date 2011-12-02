package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/*
 * FIXME: We cannot simply compare BSON symbols : https://jira.mongodb.org/browse/JAVA-479
 */
@RunWith(classOf[JUnitRunner])
class valueSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Base primitives serializer") {
    import org.bson.types.{Symbol => BsonSymbol}

    it("must set explicitly") {
      val explicit = new BaseValuePacking {}
      packValue("val")(ValueWriter.defaultWriter[String]) must equal(Some("val"))
      val sym = packValue('Sym)(explicit.symbolSetter)
      sym must be('defined)
      sym.get.asInstanceOf[AnyRef].getClass must equal(classOf[BsonSymbol])
      sym.get.asInstanceOf[BsonSymbol].getSymbol must equal("Sym")
    }
    it("must set") {
      packValue(10) must equal(Some(new java.lang.Integer(10)))
      val sym = packValue('Sym)
      sym must be('defined)
      sym.get.asInstanceOf[AnyRef].getClass must equal(classOf[BsonSymbol])
      sym.get.asInstanceOf[BsonSymbol].getSymbol must equal("Sym")
    }
    it("must get") {
      unpackValue[String]("string") must equal(Some("string"))
      unpackValue[Symbol](new BsonSymbol("Sym")) must equal(Some('Sym))
      unpackValue[Symbol]("sym") must equal(None)
      unpackValue[String]('Sym) must equal(None)
    }
  }
  describe("Recovering primitives serializer") {
    val explicit = new RecoveringValuePacking {}

    it("recovers Int") {
      unpackValue[Int](11)(explicit.intGetter) must equal(Some(11))
      unpackValue[Int]("10")(explicit.intGetter) must equal(Some(10))
      unpackValue[Int](109L)(explicit.intGetter) must equal(Some(109))
      unpackValue[Int]("x13")(explicit.intGetter) must equal(None)
    }
    it("recovers Long") {
      unpackValue[Long](11)(explicit.longGetter) must equal(Some(11L))
      unpackValue[Long]("10")(explicit.longGetter) must equal(Some(10L))
      unpackValue[Long](109L)(explicit.longGetter) must equal(Some(109L))
      unpackValue[Long]("x13")(explicit.longGetter) must equal(None)
    }
    it("recovers Double") {
      unpackValue[Double](11.76)(explicit.doubleGetter) must equal(Some(11.76))
      unpackValue[Double](67)(explicit.doubleGetter) must equal(Some(67.0))
      unpackValue[Double]("10.87")(explicit.doubleGetter) must equal(Some(10.87))
      unpackValue[Double](109L)(explicit.doubleGetter) must equal(Some(109.0))
      unpackValue[Double]("x13")(explicit.doubleGetter) must equal(None)
    }
    it("recovers Float") {
      unpackValue[Float](11.76F)(explicit.floatGetter) must equal(Some(11.76F))
      unpackValue[Float](67)(explicit.floatGetter) must equal(Some(67.0F))
      unpackValue[Float]("10.87")(explicit.floatGetter) must equal(Some(10.87F))
      unpackValue[Float](109L)(explicit.floatGetter) must equal(Some(109.0F))
      unpackValue[Float]("x13")(explicit.floatGetter) must equal(None)
    }
    it("recovers Date") {
      import java.util.Date
      // round to seconds
      val now = {
        val d = new Date
        d.setTime(d.getTime/1000 * 1000L)
        d
      }

      unpackValue[Date](now)(explicit.dateGetter) must equal(Some(now))
      unpackValue[Date]((now.getTime/1000).intValue)(explicit.dateGetter) must equal(Some(now))
      unpackValue[Date](now.getTime)(explicit.dateGetter) must equal(Some(now))
    }
  }
}
