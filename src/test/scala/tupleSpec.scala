package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}

@RunWith(classOf[JUnitRunner])
class tupleSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
  import RecoveringValuePacking._
  describe("Tuple deserializer") {
    it("deserializes Tuple2") {
      val T2 = "i".fieldOf[Int] ~ "s".fieldOf[String]
      T2.unapply(BasicDBObjectBuilder.start.get) must equal(None)
      T2.unapply(BasicDBObjectBuilder.start("i", 10).get) must equal(None)
      T2.unapply(BasicDBObjectBuilder.start("i", 10).add("s", "str").get) must equal(Some(10 -> "str"))
      T2.unapply(BasicDBObjectBuilder.start("i", "10").add("s", "str").get) must equal(Some(10 -> "str"))
    }
    it("deserializes Tuple3") {
      val T3 = "i".fieldOf[Int] ~ "s".fieldOf[String] ~ "d".fieldOf[Double]
      T3.unapply(BasicDBObjectBuilder.start.get) must equal(None)
      T3.unapply(BasicDBObjectBuilder.start("i", 10).get) must equal(None)
      T3.unapply(BasicDBObjectBuilder.start("i", 10).add("s", "str").get) must equal(None)
      T3.unapply(BasicDBObjectBuilder.start("i", 10).add("s", "str").add("d", 1.67).get) must equal(Some( (10, "str", 1.67) ))
      T3.unapply(BasicDBObjectBuilder.start("i", "10").add("s", "str").add("d", "1.67").get) must equal(Some( (10, "str", 1.67) ))

      // or invoke extractor
      BasicDBObjectBuilder.start("i", "10").add("s", "str").add("d", "1.67").get match {
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
    import RichDBO._

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
