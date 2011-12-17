package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.DBObject

@RunWith(classOf[JUnitRunner])
class updateSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  import Implicits._
  import SmartValues._

  describe("Modification operations") {
    val i = "i".fieldOf[Int]
    val j = "j".fieldOf[Int]
    it("have $set") {
      val u = i.set(10)
      u.toString must startWith("Update")

      val dbo = Lens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must containKeyValue("i" -> 10)
    }
    it("combine") {
      val u = i.set(10) ~ j.set(3)
      u.toString must startWith("Update")

      val dbo = Lens.read[DBObject]("$set", u : DBObject)
      dbo must be('defined)
      dbo.get must (containKeyValue("i" -> 10) and containKeyValue("j" -> 3))
    }
  }
}
