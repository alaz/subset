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
package update

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.{start => dbo}

@RunWith(classOf[JUnitRunner])
class updateSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import SmartValues._

  describe("Field") {
    it("provides a positional field for update") {
      val i = "i".fieldOf[Int]
      (i.first.set(3) : DBObject) must equal(dbo.push("$set").append("i.$", 3).get)
    }
  }
  describe("Modification operators") {
    val i = "i".fieldOf[Int]
    val j = "j".fieldOf[Int]
    it("have $set") {
      val u = i.set(10)
      u.toString must startWith("Update")

      val dbo = DBObjectLens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must containKeyValue("i" -> 10)
    }
    it("has $addToSet for sequences") {
      val iaf = "arr".fieldOf[List[Int]]
      val u = iaf.addToSet(1 :: 2 :: Nil)

      val addToSet = DBObjectLens.read[DBObject]("$addToSet", u: DBObject)
      addToSet must be('defined)

      val v = DBObjectLens.read[DBObject]("arr", addToSet.get)
      v must be ('defined)

      val arr = DBObjectLens.read[Array[Int]]("$each", v.get)
      arr must be('defined)
      arr.get must equal(Array(1,2))
    }
    it("combine") {
      val u = i.set(10) ~ j.set(3)
      u.toString must startWith("Update")

      val dbo = DBObjectLens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must (containKeyValue("i" -> 10) and containKeyValue("j" -> 3))
    }
  }
  describe("Subset modification") {
    object Sub {
      val f = Field[Int]("f")
    }
    object Doc {
      val f = "f".fieldOf[Int]
      val sub = "sub".subset(Sub).of[List[Int]]
    }
    val doc = "doc".subset(Doc).of[List[Int]]

    it("builds update modifiers") {
      doc.modify{_.f.set(3)}.get must equal(
        dbo.push("$set").append("doc.f", 3).get
      )
      doc.modify{_.sub.modify{_.f.set(3)}}.get must equal(
        dbo.push("$set").append("doc.sub.f", 3).get
      )
    }
    it("stacks modifications under its operator") {
      doc.modify{d => d.sub.modify{_.f.inc(2)} ~ d.f.inc(1)}.get must equal(
        dbo.push("$inc").append("doc.sub.f", 2).append("doc.f", 1).get
      )
    }
    it("may create positional update") {
      doc.matched.modify{_.f set 3}.get must equal(
        dbo.push("$set").append("doc.$.f", 3).get
      )
      doc.modify{_.sub.matched.modify{_.f set 3}}.get must equal(
        dbo.push("$set").append("doc.sub.$.f", 3).get
      )
    }
    it("updates the first element") {
      doc.modify{_.f.first set 3}.get must equal(
        dbo.push("$set").append("doc.f.$", 3).get
      )
    }
    it("supports query in $pull") {
      doc.pullWhere{_.f > 1}.get must equal(
        dbo.push("$pull").push("doc").push("f").append("$gt", 1).get
      )
      doc.modify{_.sub.pullWhere{_.f > 1}}.get must equal(
        dbo.push("$pull").push("doc.sub").push("f").append("$gt", 1).get
      )
      doc.pullWhere{_.sub.where{_.f > 1}}.get must equal(
        dbo.push("$pull").push("doc").push("sub.f").append("$gt", 1).get
      )
    }
  }
}
