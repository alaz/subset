package com.osinka.subset
package examples

import org.scalatest.{FeatureSpec,GivenWhenThen}
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.bson.types.ObjectId
import com.mongodb.{DBObject,BasicDBObjectBuilder,WriteResult}
import BasicDBObjectBuilder.start

/** "Polymorphic model" example
 *
 * Run it using `it:test` command in `sbt`.
 */
@RunWith(classOf[JUnitRunner])
class polymorphicSpec extends FeatureSpec with GivenWhenThen with MustMatchers {
  import SmartValues._

  info("A \"polymorphic test\" shows example on how to read and write polymorphic")
  info("models")
  abstract class Abstract {}
  case class ImplB(val s: String) extends Abstract
  case class ImplC(val i: Int) extends Abstract

  info("Object ImplB is able to read and write objects of class ImplB")
  object ImplB {
    val S = "s".fieldOf[String]

    implicit val reader = ValueReader[ImplB]({
      case S(s) => new ImplB(s)
    })
    implicit val writer = ValueWriter[ImplB](b => S(b.s).get)
  }

  info("Object ImplC is able to read and write objects of class ImplC")
  object ImplC {
    val I = "i".fieldOf[Int]

    implicit val reader = ValueReader[ImplC]({
      case I(i) => new ImplC(i)
    })
    implicit val writer = ValueWriter[ImplC](c => I(c.i).get)
  }

  feature("It is possible to serialize subtypes using their common field") {
    info("There are two classes ImplB and ImplC extending one trait Abstract")
    info("Both these classes are easily distinguishable, they have")
    info("different fields")

    scenario("Storing ImplB") {
      given("there is a field of type Abstract")
      val af = "a".fieldOf[Abstract]

      when("storing ImplB")
      val b: DBObject = af( new ImplB("str") )

      then("resulting DBObject contains it's representation")
      b must equal(start.push("a").append("s", "str").get)
    }

    scenario("Storing ImplC") {
      given("there is a field of type Abstract")
      val af = "a".fieldOf[Abstract]

      when("storing ImplC")
      val c: DBObject = af( new ImplC(5) )

      then("resulting DBObject contains it's representation")
      c must equal(start.push("a").append("i", 5).get)
    }
  }

  feature("It is possible to deserialize subtypes") {
    scenario("Reading ImplB") {
      given("there is an object storing ImplB")
      val obj: DBObject = start.push("a").append("s", "str").get

      then("ImplB can read it")
      val B = "a".fieldOf[ImplB]
      PartialFunction.condOpt(obj) { case B(b) => b } must equal(Some(new ImplB("str")))

      and("ImplC cannot")
      val C = "a".fieldOf[ImplC]
      PartialFunction.condOpt(obj) { case C(c) => c } must equal(None)
    }

    scenario("Reading ImplC") {
      given("there is an object storing ImplB")
      val obj: DBObject = start.push("a").append("i", 5).get

      then("ImplC can read it")
      val C = "a".fieldOf[ImplC]
      PartialFunction.condOpt(obj) { case C(c) => c } must equal(Some(new ImplC(5)))

      and("ImplB cannot")
      val B = "a".fieldOf[ImplB]
      PartialFunction.condOpt(obj) { case B(b) => b } must equal(None)
    }
  }

  feature("It is possible to compare a field to subtypes") {
    scenario("Comparing Abstract field") {
      pending

      given("there is a field of type Abstract")
      val f = "a".fieldOf[Abstract]

      then("it's possible to query equivalence to ImplB instance")
      //(f === ImplB("str")).get must equal(start.push("a").append("s", "str").get)

      and("it's possible to query equivalence to ImplC instance")
      //(f === ImplC(5)).get must equal(start.push("a").append("i", 5).get)
    }
  }

  feature("It is possible to update a field with its subtype") {
    scenario("Updating Abstract field") {
      given("there is a field of type Abstract")
      val f = "a".fieldOf[Abstract]

      then("it's possible to update with ImplB instance")
      f.set(ImplB("str")).get must equal(start.push("$set").push("a").append("s", "str").get)

      then("it's possible to update with ImplC instance")
      f.set(ImplC(5)).get must equal(start.push("$set").push("a").append("i", 5).get)
    }
  }
}
