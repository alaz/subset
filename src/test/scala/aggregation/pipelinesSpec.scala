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
package aggregation

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.BasicDBObjectBuilder
import BasicDBObjectBuilder.{start => dbo}

@RunWith(classOf[JUnitRunner])
class pipelinesSpec extends FunSpec with ShouldMatchers with MongoMatchers {
  describe("Project") {
    it("accepts all fields") {
      Project.all("a".fieldOf[String]) should equal(dbo.push("$project").add("a", 1).get)
      Project.all("a".fieldOf[String], "b".fieldOf[String]) should equal(dbo.push("$project").add("a", 1).add("b", 1).get)
    }
    it("lets select fields") {
      Project("a".fieldOf[String] === Project.Include) should equal(dbo.push("$project").add("a", 1).get)
      Project(
        "a".fieldOf[String] === Project.Exclude &&
        "b".fieldOf[String] === Project.Include) should
        equal(dbo.push("$project").add("a", 0).add("b", 1).get)
    }
    it("lets project to a doc") {
      object Doc {
        val pv = "pv".fieldOf[Int]
        val p2 = "p2".fieldOf[Int]
      }
      val doc = "stats".subset(Doc).of[Unit]
      val f1 = "f1".fieldOf[Int]
      val f2 = "f2".fieldOf[Int]

      Project(doc build {d => d.pv === f1 && d.p2 === f2}) should equal(
        dbo.push("$project")
          .push("stats")
          .add("pv", "$f1").add("p2", "$f2")
          .get
      )
    }
  }
  describe("Project operators") {
    they("support mix of fields and values") {
      pending // until https://jira.mongodb.org/browse/JAVA-482

      val result = "r".fieldOf[Int]
      val f = "f".fieldOf[Int]
      Project(result === Project.Add(f, 12, "str")) should equal(
        dbo.push("$project")
          .push("r")
          .add("$add", Array("$f", 12, "str"))
          .get
      )
    }
  }
  describe("Match") {
    it("accepts query") { pending }
  }
  describe("Limit") {
    it("works") { pending }
  }
  describe("Skip") {
    it("works") { pending }
  }
  describe("Unwind") {
    it("accepts field") {
      Unwind("f".fieldOf[List[Int]]) should equal(dbo("$unwind", "$f").get)
    }
    it("accepts dotted field") {
      val doc = "doc".subset(()).of[Unit]
      val field = "f".fieldOf[List[Int]]
      Unwind(field in doc) should equal(dbo("$unwind", "$doc.f").get)
    }
  }
  describe("Group") {
    it("last") {
      val id = "_id".subset(()).of[Unit]
      val state = "state".fieldOf[String]
      val city = "city".fieldOf[String]

      Group(id,
        "biggestCity".fieldOf[String] -> Group.Last(city in id),
        "smallestCity".fieldOf[String] -> Group.First(city in id)) should equal(
        dbo.push("$group")
          .add("_id", "$_id")
          .push("biggestCity").add("$last", "$_id.city").pop
          .push("smallestCity").add("$first", "$_id.city").pop
          .get
      )
    }
    it("document as id") {
      // http://docs.mongodb.org/manual/tutorial/aggregation-examples/#average-city-population-by-state
      val state = "state".fieldOf[String]
      val city = "city".fieldOf[String]
      val pop = "pop".fieldOf[Long]
      val id = "_id".subset(()).of[Unit]

      Group(id.build{_ => state === state && city === city},
        pop -> Group.Sum(pop)) should equal(
        dbo.push("$group")
          .push("_id")
            .add("state", "$state")
            .add("city", "$city")
            .pop
          .push("pop")
            .add("$sum", "$pop")
          .get
      )
    }
  }
}
