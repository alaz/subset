package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.util.regex.Pattern
import org.bson.BSON
import com.mongodb.{DBObject,BasicDBObjectBuilder,QueryBuilder}
import QueryBuilder.{start => query}
import BasicDBObjectBuilder.{start => dbo}

@RunWith(classOf[JUnitRunner])
class querySpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
  import RecoveringValuePacking._

  describe("field query") {
    val i = "i".fieldOf[Int]

    it("has $exists") {
      (i exists true).get must equal(query("i").exists(true).get)
      (i exists false).get must equal(query("i").exists(false).get)
    }
    it("has $eq") {
      (i === 10).get must equal(dbo("i", 10).get)
      (i === None).get must equal(query("i").exists(false).get)
      (i === Some(10)).get must equal(dbo("i", 10).get)
      (i === "^str".r).get must equal(dbo("i", Pattern.compile("^str")).get)
      (i === Pattern.compile("^str")).get must equal(dbo("i", Pattern.compile("^str")).get)
    }
    it("has $ne") {
      (i !== 10).get must equal(query("i").notEquals(10).get)
    }
    it("has $gt") {
      (i > 10).get must equal(query("i").greaterThan(10).get)
    }
    it("has $gte") {
      (i >= 10).get must equal(query("i").greaterThanEquals(10).get)
    }
    it("has $lt") {
      (i < 10).get must equal(query("i").lessThan(10).get)
    }
    it("has $lte") {
      (i <= 10).get must equal(query("i").lessThanEquals(10).get)
    }
    it("has $size") {
      (i size 10).get must equal(query("i").size(10).get)
    }
    it("has $type") {
      (i `type` BSON.NUMBER).get must equal(dbo.push("i").add("$type", BSON.NUMBER).get)
    }
    it("has $in") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      // (i in List(1,2)).get must equal(query("i").in(Array(1,2)).get)
      import Implicits._
      import StrictValuePacking._
      import RichDBO._

      val dbo = (i in List(1,2)).get

      val in = dbo.read[DBObject]("i")
      in must be('defined)

      val arr = in.get.read[Array[Int]]("$in")
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $all") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i all List(1,2)).get must equal(query("i").all(Array(1,2)).get)
      import Implicits._
      import StrictValuePacking._
      import RichDBO._

      val dbo = (i all Array(1,2)).get

      val in = dbo.read[DBObject]("i")
      in must be('defined)

      val arr = in.get.read[Array[Int]]("$all")
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $nin") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i notIn List(1,2)).get must equal(query("i").notIn(Array(1,2)).get)
      import Implicits._
      import StrictValuePacking._
      import RichDBO._

      val dbo = (i notIn Iterable(1,2)).get

      val in = dbo.read[DBObject]("i")
      in must be('defined)

      val arr = in.get.read[Array[Int]]("$nin")
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $mod") {
      pendingUntilFixed {
        // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
        i.mod(10, 1).get must equal(query("i").mod(10, 1).get)
      }
    }
    it("has $near") {
      pendingUntilFixed {
        // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
        i.near(2.5, 6.7).get must equal(query("i").near(2.5, 6.7).get)
        i.near(2.5, 6.7, 3.4).get must equal(query("i").near(2.5, 6.7, 3.4).get)
      }
    }
    it("has $not") {
      val q = !(i < 10)
      q.toString must startWith("Query")
      q.get must equal(dbo.push("i").push("$not").add("$lt", 10).get)
    }
    it("accumulates") {
      (i < 10 > 5).get must equal(query("i").lessThan(10).greaterThan(5).get)
      (i < 10 > 5 <= 15 < 5).get must equal(query("i").lessThan(10).greaterThan(5).lessThanEquals(15).lessThan(5).get)
    }
  }
  describe("query") {
    import RichDBO._

    it("supports conjunction") {
      val dbo: DBObject = "i".fieldOf[Int] < 10 && "k".fieldOf[Int] > 4 && "m".fieldOf[Int] === 3
      dbo must containKeyValue("m" -> 3)
      dbo.read[DBObject]("i") must be('defined)
      dbo.read[DBObject]("i").get must containKeyValue("$lt" -> 10)
      dbo.read[DBObject]("k") must be('defined)
      dbo.read[DBObject]("k").get must containKeyValue("$gt" -> 4)
    }
    it("supports $or") {
      val dbo: DBObject = "i".fieldOf[Int] === 10 || "k".fieldOf[Int] === 4 or "m".fieldOf[Int] === 7
      val arr = dbo.read[Array[DBObject]]("$or")
      arr must be('defined)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
    it("supports $nor") {
      val dbo: DBObject = "i".fieldOf[Int] === 10 nor "k".fieldOf[Int] === 4 nor "m".fieldOf[Int] === 7
      val arr = dbo.read[Array[DBObject]]("$nor")
      arr must be('defined)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
  }
}
