/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.osinka.subset
package query

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
  import SmartValues._

  describe("Field query") {
    val i = "i".fieldOf[Int]
    val ai = "i".fieldOf[List[Int]]

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
      (ai size 10).get must equal(query("i").size(10).get)
    }
    it("has $type") {
      (i `type` BSON.NUMBER).get must equal(dbo.push("i").add("$type", BSON.NUMBER).get)
    }
    it("has $in") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      // (i in List(1,2)).get must equal(query("i").in(Array(1,2)).get)

      val dbo = (i in List(1,2)).get

      val in = DBObjectLens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = DBObjectLens.read[Array[Int]]("$in", in.get)
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $all") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i all List(1,2)).get must equal(query("i").all(Array(1,2)).get)

      val dbo = (ai all Array(1,2)).get

      val in = DBObjectLens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = DBObjectLens.read[Array[Int]]("$all", in.get)
      arr must be ('defined)
      arr.get must equal(Array(1,2))
    }
    it("has $nin") {
      // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
      //(i notIn List(1,2)).get must equal(query("i").notIn(Array(1,2)).get)

      val dbo = (i notIn Iterable(1,2)).get

      val in = DBObjectLens.read[DBObject]("i", dbo)
      in must be('defined)

      val arr = DBObjectLens.read[Array[Int]]("$nin", in.get)
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
    it("makes ranges") {
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
      DBObjectLens.read[DBObject]("i", dbo) must be('defined)
      DBObjectLens.read[DBObject]("i", dbo).get must containKeyValue("$lt" -> 10)
      DBObjectLens.read[DBObject]("k", dbo) must be('defined)
      DBObjectLens.read[DBObject]("k", dbo).get must containKeyValue("$gt" -> 4)
    }
    it("supports conjunction w/ $and, in one order") {
      val dbo: DBObject = i === 4 && i > 5 && k === 5
      val arr = DBObjectLens.read[Array[DBObject]]("$and", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 4)
      arr.get(1) must containField("i")
      arr.get(2) must containKeyValue("k" -> 5)
    }
    it("supports conjunction w/ $and, in another order") {
      val dbo: DBObject = i === 4 && k === 5 && i > 5
      val arr = DBObjectLens.read[Array[DBObject]]("$and", dbo)
      arr must be('defined)
      arr.get.size must equal(2)
      arr.get(0) must (containKeyValue("i" -> 4) and containKeyValue("k" -> 5))
      arr.get(1) must containField("i")
    }
    it("supports $or") {
      val dbo: DBObject = i === 10 || k === 4 or m === 7
      val arr = DBObjectLens.read[Array[DBObject]]("$or", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
    it("supports $nor") {
      val dbo: DBObject = i === 10 nor k === 4 nor m === 7
      val arr = DBObjectLens.read[Array[DBObject]]("$nor", dbo)
      arr must be('defined)
      arr.get.size must equal(3)
      arr.get(0) must containKeyValue("i" -> 10)
      arr.get(1) must containKeyValue("k" -> 4)
      arr.get(2) must containKeyValue("m" -> 7)
    }
  }
  describe("Subset query") {
    object Sub {
      val f = Field[Int]("f")
    }
    object Doc {
      val f = "f".fieldOf[Int]
      val sub = "sub".subset(Sub).of[DBObject]
    }
    val doc = "doc".subset(Doc).of[DBObject]

    // a field alias, in case of frequent queries
    val alias = Doc.f.in(Doc.sub).in(doc)

    it("writes long names") {
      (doc.where{_.f === 10}).get must equal(dbo("doc.f", 10).get)
      (doc.where{_.sub.where {_.f > 10}}).get must equal(
        dbo.push("doc.sub.f").append("$gt", 10).get
      )
      
      (alias > 10).get must equal(
        dbo.push("doc.sub.f").append("$gt", 10).get
      )
    }
    it("supports conjunction") {
      doc.where{d => d.f <= 10 && d.sub.where{_.f === 3}}.get must equal(
        dbo.
          push("doc.f").
          append("$lte", 10).
          pop.
          append("doc.sub.f", 3).get
      )
    }
    it("supports $elemMatch") {
      doc.elemMatch{_.f > 10}.get must equal(
        dbo.
          push("doc").
          push("$elemMatch").
          push("f").
          append("$gt", 10).get
      )
      doc.where{_.sub.elemMatch{_.f === 3}}.get must equal(
        dbo.
          push("doc.sub").
          push("$elemMatch").
          append("f", 3).get
      )
    }
    it("builds positional query") {
      doc(0).where{_.f === 3}.get must equal(
        dbo("doc.0.f", 3).get
      )
      doc.where{_.sub(1).where{_.f > 5}}.get must equal(
        dbo.push("doc.sub.1.f").append("$gt", 5).get
      )
    }
    it("honors top-level queries like $and, $or") {
      val q1 = doc.where{_.sub.where{s => s.f > 3 && s.f < 2}}.get
      q1 must containField("$and")
      val andArr = DBObjectLens.read[List[DBObject]]("$and", q1)
      andArr must be('defined)
      andArr.get must (
          contain(dbo.push("doc.sub.f").append("$gt", 3).get) and
          contain(dbo.push("doc.sub.f").append("$lt", 2).get)
        )

      val q2 = doc.where{_.sub.where{s => s.f > 3 || s.f < 2}}.get
      q2 must containField("$or")
      val orArr = DBObjectLens.read[List[DBObject]]("$or", q2)
      orArr must be('defined)
      orArr.get must (
          contain(dbo.push("doc.sub.f").append("$gt", 3).get) and
          contain(dbo.push("doc.sub.f").append("$lt", 2).get)
        )
    }
  }
}
