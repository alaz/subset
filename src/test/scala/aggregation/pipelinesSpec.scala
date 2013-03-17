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
      Project("a".fieldOf[String] -> 1) should equal(dbo.push("$project").add("a", 1).get)
      Project("a".fieldOf[String] -> 0, "b".fieldOf[String] -> 1) should equal(dbo.push("$project").add("a", 0).add("b", 1).get)
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
      val field = "f".fieldOf[List[Int]].in(doc)
      Unwind(field) should equal(dbo("$unwind", "$doc.f").get)
    }
  }
  describe("Group") {
    it("last") {
      val id = "_id".subset(()).of[Unit]
      val state = "state".fieldOf[String].in(id)
      val city = "city".fieldOf[String].in(id)

      Group(id,
        "biggestCity".fieldOf[String] -> Group.Last(city),
        "smallestCity".fieldOf[String] -> Group.First(city)) should equal(
        dbo.push("$group")
          .add("_id", "$_id")
          .push("biggestCity").add("$last", "$_id.city").pop
          .push("smallestCity").add("$first", "$_id.city").pop
          .get
      )
    }
  }
}
