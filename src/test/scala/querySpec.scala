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
  import SmartValues._

  describe("Field query") {
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

      val dbo = (i in List(1,2)).get

      val in = Lens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = Lens.read[Array[Int]]("$in", in.get)
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $all") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i all List(1,2)).get must equal(query("i").all(Array(1,2)).get)

      val dbo = (i all Array(1,2)).get

      val in = Lens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = Lens.read[Array[Int]]("$all", in.get)
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $nin") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i notIn List(1,2)).get must equal(query("i").notIn(Array(1,2)).get)

      val dbo = (i notIn Iterable(1,2)).get

      val in = Lens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = Lens.read[Array[Int]]("$nin", in.get)
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
  describe("Combining query") {
    val i = "i".fieldOf[Int]
    val k = "k".fieldOf[Int]
    val m = "m".fieldOf[Int]

    it("supports conjunction w/o $and") {
      val dbo: DBObject = i < 10 && k > 4 && m === 3
      dbo must containKeyValue("m" -> 3)
      Lens.read[DBObject]("i", dbo) must be('defined)
      Lens.read[DBObject]("i", dbo).get must containKeyValue("$lt" -> 10)
      Lens.read[DBObject]("k", dbo) must be('defined)
      Lens.read[DBObject]("k", dbo).get must containKeyValue("$gt" -> 4)
    }
    it("supports conjunction w/ $and, in one order") {
      val dbo: DBObject = i === 4 && i > 5 && k === 5
      val arr = Lens.read[Array[DBObject]]("$and", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 4)
      arr.get(1) must containField("i")
      arr.get(2) must containKeyValue("k" -> 5)
    }
    it("supports conjunction w/ $and, in another order") {
      val dbo: DBObject = i === 4 && k === 5 && i > 5
      val arr = Lens.read[Array[DBObject]]("$and", dbo)
      arr must be('defined)
      arr.get.size must equal(2)
      arr.get(0) must (containKeyValue("i" -> 4) and containKeyValue("k" -> 5))
      arr.get(1) must containField("i")
    }
    it("supports $or") {
      val dbo: DBObject = i === 10 || k === 4 or m === 7
      val arr = Lens.read[Array[DBObject]]("$or", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
    it("supports $nor") {
      val dbo: DBObject = i === 10 nor k === 4 nor m === 7
      val arr = Lens.read[Array[DBObject]]("$nor", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
  }
  describe("Subset query") {
    object Doc extends Subset("doc") {
      val f = "f".fieldOf[Int]

      object Sub extends Subset("sub") {
        val f = Field[Int]("f")
      }
    }

    it("writes long names") {
      (Doc.f === 10).get must equal(dbo("doc.f", 10).get)
      (Doc.Sub.f === 10).get must equal(dbo("doc.sub.f", 10).get)
    }
    it("supports conjunction") {
      (Doc.f === 10 && Doc.Sub.f === 3).get must equal(dbo("doc.f", 10).append("doc.sub.f", 3).get)
    }
    it("supports $elemMatch") {
      Doc.elemMatch(doc => doc.f === 10).get must equal(dbo.push("doc").push("$elemMatch").append("f", 10).get)
      Doc.Sub.elemMatch(doc => doc.f === 3).get must equal(dbo.push("doc.sub").push("$elemMatch").append("f", 3).get)
    }
  }
}
