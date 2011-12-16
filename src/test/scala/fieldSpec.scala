package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class fieldSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
  import SmartValues._

  describe("Field") {
    it("serializes explicitly") {
      val f = Field[Int]("i")
      (f(10) : DBObject) must containKeyValue("i" -> 10)
    }
    it("serializes implicitly") {
      val f = Field[Int]("i")
      val dbo: DBObject = f -> 10
      dbo must containKeyValue("i" -> 10)
    }
    it("chains serializers 1") {
      val f1 = "i".fieldOf[Int]
      val f2 = "s".fieldOf[String]
      ( (f1 -> 12) ~ (f2 -> "string") : DBObject) must (containKeyValue("i" -> 12) and containKeyValue("s" -> "string"))
    }
    it("chains serializers 2") {
      val obj = ("i".fieldOf[Int] -> 12) ~ ("s".fieldOf[String] -> "string")
      (obj : DBObject) must (containKeyValue("i" -> 12) and containKeyValue("s" -> "string"))
    }
    it("has extractor") {
      val F1 = "i".fieldOf[Int]
      start("i", 10).get match {
        case F1(i) => i must equal(10)
        case _ => fail("must extract field value")
      }
    }
    it("has conjunction extractor") {
      val O = "i".fieldOf[Int] ~ "s".fieldOf[String]
      start("i", 10).append("s", "string").get match {
        case O(i, s) =>
          i must equal(10)
          s must equal("string")
        case _ =>
          fail("must extract field value")
      }
    }
  }
}
