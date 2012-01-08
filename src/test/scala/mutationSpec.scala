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

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class mutationSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Mutation") {
    // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
    def dbo = start("i", 10).push("inner").add("s", "string")/*.add("a", Array(1,2))*/.get
    def constMutation = Mutation.const(dbo)
    def identityMutation = Mutation(identity[DBObject] _)

    it("has `equals`") {
      constMutation must equal(constMutation)
      constMutation.get must equal(dbo)
    }
    it("has `hashCode`") {
      constMutation.hashCode must equal(constMutation.hashCode)
      constMutation.get.hashCode must equal(dbo.hashCode)
    }
    it("has `toString`") {
      constMutation.toString must startWith("Mutation")
    }
    it("initialized from empty DBObject") {
      identityMutation.get must be('empty)
    }
    it("has ValueWriter") {
      val w = implicitly[ValueWriter[Mutation]]
    }
    it("allows conjunction") {
      val conj = constMutation ~ identityMutation
      conj.toString must startWith("Mutation")
      conj must equal(constMutation)
    }
    it("supports Scala methods") {
      (constMutation andThen identityMutation)(dbo) must equal(dbo)
    }
    it("supports DSL") {
      val set1 = Mutation.writer[Int]("i", 1)
      val set2 = Mutation.writer[Int]("i", 2)
      (set1 ~ set2 :~> dbo) must containKeyValue("i" -> 2)
      (dbo <~: set1 ~ set2) must containKeyValue("i" -> 2)
    }
  }
  describe("Writer mutation") {
    def w = Mutation.writer("i", 10)
    it("writes into empty object") {
      w(start.get) must equal(start("i", 10).get)
      w(w(start.get)) must equal(start("i", 10).get)
    }
    it("appends to an existing object") {
      w(start("a", "str").get) must equal(start("a", "str").append("i", 10).get)
    }
    it("overwrites old value in an obejct") {
      w(start("a", "str").append("i", 5).get) must equal(start("a", "str").append("i", 10).get)
      w(w(start("a", "str").append("i", 5).get)) must equal(start("a", "str").append("i", 10).get)
    }
  }
  describe("Remover mutation") {
    def r = Mutation.remover("i")
    it("keeps old object intact") {
      r(start.get) must equal(start.get)
      r(start("s", "str").get) must equal(start("s", "str").get)
    }
    it("removes the key only") {
      r(start("i", 5).append("s", "str").get) must equal(start("s", "str").get)
      r(start("i", 5).get) must equal(start.get)
    }
  }
  describe("Modifier mutation") {
    def m = Mutation.modifier("i", (i: Int) => (i+1).toString)
    it("keeps an object intact") {
      m(start.get) must equal(start.get)
      m(start("s", "str").get) must equal(start("s", "str").get)
    }
    it("modifies only the key") {
      m(start("i",5).get) must equal(start("i","6").get)
      m(start("i",5).append("s","str").get) must equal(start("i","6").append("s", "str").get)
    }
  }
}
