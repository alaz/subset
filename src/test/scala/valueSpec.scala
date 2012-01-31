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

import com.mongodb.{BasicDBList,BasicDBObjectBuilder}
import BasicDBObjectBuilder.{start => dbo}

/*
 * FIXME: We cannot simply compare BSON symbols : https://jira.mongodb.org/browse/JAVA-479
 */
@RunWith(classOf[JUnitRunner])
class valueSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Base primitives serializer") {
    import org.bson.types.{Symbol => BsonSymbol}

    it("must set explicitly") {
      packValue("val")(ValueWriter.stringSetter) must equal(Some("val"))
      val sym = packValue('Sym)(ValueWriter.symbolSetter)
      sym must be('defined)
      sym.get.asInstanceOf[AnyRef].getClass must equal(classOf[BsonSymbol])
      sym.get.asInstanceOf[BsonSymbol].getSymbol must equal("Sym")
    }
  }
  describe("Option writer") {
    it("sets Some") {
      packValue[Option[Int]](Some(1)) must equal(Some(1))
    }
    it("sets None") {
      packValue[Option[Int]](None: Option[Int]) must be('empty)
    }
  }
  describe("Option reader") {
    it("never returns None") {
      unpackValue[Option[Int]](1) must equal(Some(Some(1)))
      unpackValue[Option[Int]]("str") must equal(Some(None))
    }
  }
  describe("List writer") {
    it("sets empty List[T]") {
      val o = packValue(Nil: List[Int])
      o must be('defined)
      o.get must equal(Array[Int]())
    }
    it("sets non-empty List[T]") {
      val o = packValue(List(1,2))
      o must be('defined)
      o.get must equal(Array(1,2))
    }
    it("sets Iterable[T]") {
      val o = packValue(Iterable(1,2))
      o must be('defined)
      o.get must equal(Array(1,2))
    }
  }
  describe("List reader") {
    it("gets List[T] from BSON DBList") {
      val da = new BasicDBList
      da.put(0, 1)
      da.put(1, 2)
      da.put(2, 3)
      unpackValue[List[Int]](da) must equal(Some(List(1,2,3)))
    }
    it("gets List[T] from Array") {
      unpackValue[List[Int]](Array(1,2,3)) must equal(Some(List(1,2,3)))
    }
  }
  describe("Tuple writer") {
    it("sets Tuple2") {
      packValue("s" -> 10).map{_.asInstanceOf[Array[_]].toList} must equal(Some(Array("s", 10).toList))
    }
    it("gets Tuple2") {
      unpackValue[Tuple2[String,Int]](Array("s", 10)) must equal(Some("s" -> 10))
    }
  }
  describe("Recovering primitives serializer") {
    import org.bson.types.{ObjectId, Symbol => BsonSymbol}
    val explicit = SmartValues

    it("must set") {
      import explicit._
      packValue(10) must equal(Some(new java.lang.Integer(10)))
      val sym = packValue('Sym)
      sym must be('defined)
      sym.get.asInstanceOf[AnyRef].getClass must equal(classOf[BsonSymbol])
      sym.get.asInstanceOf[BsonSymbol].getSymbol must equal("Sym")
    }
    it("must get") {
      import explicit._
      unpackValue[String]("string") must equal(Some("string"))
      unpackValue[Symbol](new BsonSymbol("Sym")) must equal(Some('Sym))
      unpackValue[Symbol]("sym") must equal(None)
      unpackValue[String]('Sym) must equal(None)
    }
    it("recovers ObjectId") {
      unpackValue[ObjectId](11)(explicit.objIdRecoveringGetter) must equal(None)
      unpackValue[ObjectId]("11")(explicit.objIdRecoveringGetter) must equal(None)
      unpackValue[ObjectId](new ObjectId)(explicit.objIdRecoveringGetter) must be('defined)
      unpackValue[ObjectId]( (new ObjectId).toString )(explicit.objIdRecoveringGetter) must be('defined)
    }
    it("recovers Int") {
      unpackValue[Int](11)(explicit.intRecoveringGetter) must equal(Some(11))
      unpackValue[Int]("10")(explicit.intRecoveringGetter) must equal(Some(10))
      unpackValue[Int](109L)(explicit.intRecoveringGetter) must equal(Some(109))
      unpackValue[Int]("x13")(explicit.intRecoveringGetter) must equal(None)
    }
    it("recovers Long") {
      unpackValue[Long](11)(explicit.longRecoveringGetter) must equal(Some(11L))
      unpackValue[Long]("10")(explicit.longRecoveringGetter) must equal(Some(10L))
      unpackValue[Long](109L)(explicit.longRecoveringGetter) must equal(Some(109L))
      unpackValue[Long]("x13")(explicit.longRecoveringGetter) must equal(None)
    }
    it("recovers Double") {
      unpackValue[Double](11.76)(explicit.doubleRecoveringGetter) must equal(Some(11.76))
      unpackValue[Double](67)(explicit.doubleRecoveringGetter) must equal(Some(67.0))
      unpackValue[Double]("10.87")(explicit.doubleRecoveringGetter) must equal(Some(10.87))
      unpackValue[Double](109L)(explicit.doubleRecoveringGetter) must equal(Some(109.0))
      unpackValue[Double]("x13")(explicit.doubleRecoveringGetter) must equal(None)
    }
    it("recovers Float") {
      unpackValue[Float](67)(explicit.floatRecoveringGetter) must equal(Some(67.0F))
      unpackValue[Float]("10.87")(explicit.floatRecoveringGetter) must equal(Some(10.87F))
      unpackValue[Float](109L)(explicit.floatRecoveringGetter) must equal(Some(109.0F))
      unpackValue[Float]("x13")(explicit.floatRecoveringGetter) must equal(None)
    }
    it("recovers Date") {
      import java.util.Date
      // round to seconds
      val now = {
        val d = new Date
        d.setTime(d.getTime/1000 * 1000L)
        d
      }

      unpackValue[Date](now)(explicit.dateRecoveringGetter) must equal(Some(now))
      unpackValue[Date]((now.getTime/1000).intValue)(explicit.dateRecoveringGetter) must equal(Some(now))
      unpackValue[Date](now.getTime)(explicit.dateRecoveringGetter) must equal(Some(now))
    }
  }
}
