package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import BasicDBObjectBuilder.{start => dbo}

@RunWith(classOf[JUnitRunner])
class updateSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
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

      val dbo = Lens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must containKeyValue("i" -> 10)
    }
    it("has $addToSet for sequences") {
      val u = i.addToSet(1 :: 2 :: Nil)

      val addToSet = Lens.read[DBObject]("$addToSet", u: DBObject)
      addToSet must be('defined)

      val v = Lens.read[DBObject]("i", addToSet.get)
      v must be ('defined)

      val arr = Lens.read[Array[Int]]("$each", v.get)
      arr must be('defined)
      arr.get must equal(Array(1,2))
    }
    it("combine") {
      val u = i.set(10) ~ j.set(3)
      u.toString must startWith("Update")

      val dbo = Lens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must (containKeyValue("i" -> 10) and containKeyValue("j" -> 3))
    }
  }
  describe("Subset modification") {
    object Doc extends Subset("doc") {
      val f = "f".fieldOf[Int]

      object Sub extends Subset("sub") {
        val f = Field[Int]("f")
      }
    }

    it("supports positional $ operator") {
      Doc.updateMatch(doc => doc.f.set(3)).get must equal(dbo.push("$set").append("doc.$.f", 3).get)
      Doc.Sub.updateMatch(doc => doc.f.set(3)).get must equal(dbo.push("$set").append("doc.sub.$.f", 3).get)
    }
  }
}
