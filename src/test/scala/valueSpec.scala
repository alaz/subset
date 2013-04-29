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

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.{BasicDBList,BasicDBObjectBuilder}
import BasicDBObjectBuilder.{start => dbo}

@RunWith(classOf[JUnitRunner])
class valueSpec extends FunSpec with MustMatchers with MongoMatchers {
  describe("Base primitives serializer") {
    import org.bson.types.{Symbol => BsonSymbol}

    it("must set explicitly") {
      ValueWriter.pack("val")(ValueWriter.stringSetter) must equal(Some("val"))
      val sym = ValueWriter.pack('Sym)(ValueWriter.symbolSetter)
      sym must equal(Some(new BsonSymbol("Sym")))
    }
  }
  describe("Option writer") {
    it("sets Some") {
      ValueWriter.pack[Option[Int]](Some(1)) must equal(Some(1))
    }
    it("sets None") {
      ValueWriter.pack[Option[Int]](None: Option[Int]) must be('empty)
    }
  }
  describe("Option reader") {
    it("never returns None") {
      ValueReader.unpack[Option[Int]](1) must equal(Some(Some(1)))
      ValueReader.unpack[Option[Int]]("str") must equal(Some(None))
    }
  }
  describe("List writer") {
    it("sets empty List[T]") {
      val o = ValueWriter.pack(Nil: List[Int])
      o must be('defined)
      o.get must equal(Array[Int]())
    }
    it("sets non-empty List[T]") {
      val o = ValueWriter.pack(List(1,2))
      o must be('defined)
      o.get must equal(Array(1,2))
    }
    it("sets Iterable[T]") {
      val o = ValueWriter.pack(Iterable(1,2))
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
      ValueReader.unpack[List[Int]](da) must equal(Some(List(1,2,3)))
    }
    it("gets List[T] from Array") {
      ValueReader.unpack[List[Int]](Array(1,2,3)) must equal(Some(List(1,2,3)))
    }
  }
  describe("Array reader") {
    it("gets Array[T] from BSON DBList") {
      val da = new BasicDBList
      da.put(0, 1)
      da.put(1, 2)
      da.put(2, 3)
      val opt = ValueReader.unpack[Array[Int]](da)
      opt must be('defined)
      opt.get must equal(Array(1,2,3))
    }
    it("gets Array[T] from Array") {
      val opt = ValueReader.unpack[Array[Int]](Array(1,2,3))
      opt must be('defined)
      opt.get must equal(Array(1,2,3))
    }
  }
  describe("Tuple writer") {
    it("sets Tuple2") {
      ValueWriter.pack("s" -> 10).map{_.asInstanceOf[Array[_]].toList} must equal(Some(Array("s", 10).toList))
    }
    it("gets Tuple2") {
      ValueReader.unpack[Tuple2[String,Int]](Array("s", 10)) must equal(Some("s" -> 10))
    }
  }
  describe("Recovering primitives serializer") {
    import org.bson.types.{ObjectId, Symbol => BsonSymbol}
    val explicit = SmartValues

    it("must set") {
      import explicit._
      ValueWriter.pack(10) must equal(Some(new java.lang.Integer(10)))
      val sym = ValueWriter.pack('Sym)
      sym must equal(Some(new BsonSymbol("Sym")))
    }
    it("must get") {
      import explicit._
      ValueReader.unpack[String]("string") must equal(Some("string"))
      ValueReader.unpack[Symbol](new BsonSymbol("Sym")) must equal(Some('Sym))
      ValueReader.unpack[Symbol]("sym") must equal(None)
      ValueReader.unpack[String]('Sym) must equal(None)
    }
    it("recovers ObjectId") {
      ValueReader.unpack[ObjectId](11)(explicit.objIdRecoveringGetter) must equal(None)
      ValueReader.unpack[ObjectId]("11")(explicit.objIdRecoveringGetter) must equal(None)
      ValueReader.unpack[ObjectId](new ObjectId)(explicit.objIdRecoveringGetter) must be('defined)
      ValueReader.unpack[ObjectId]( (new ObjectId).toString )(explicit.objIdRecoveringGetter) must be('defined)
    }
    it("recovers Int") {
      ValueReader.unpack[Int](11)(explicit.intRecoveringGetter) must equal(Some(11))
      ValueReader.unpack[Int]("10")(explicit.intRecoveringGetter) must equal(Some(10))
      ValueReader.unpack[Int](109L)(explicit.intRecoveringGetter) must equal(Some(109))
      ValueReader.unpack[Int]("x13")(explicit.intRecoveringGetter) must equal(None)
    }
    it("recovers Long") {
      ValueReader.unpack[Long](11)(explicit.longRecoveringGetter) must equal(Some(11L))
      ValueReader.unpack[Long]("10")(explicit.longRecoveringGetter) must equal(Some(10L))
      ValueReader.unpack[Long](109L)(explicit.longRecoveringGetter) must equal(Some(109L))
      ValueReader.unpack[Long]("x13")(explicit.longRecoveringGetter) must equal(None)
    }
    it("recovers Double") {
      ValueReader.unpack[Double](11.76)(explicit.doubleRecoveringGetter) must equal(Some(11.76))
      ValueReader.unpack[Double](67)(explicit.doubleRecoveringGetter) must equal(Some(67.0))
      ValueReader.unpack[Double]("10.87")(explicit.doubleRecoveringGetter) must equal(Some(10.87))
      ValueReader.unpack[Double](109L)(explicit.doubleRecoveringGetter) must equal(Some(109.0))
      ValueReader.unpack[Double]("x13")(explicit.doubleRecoveringGetter) must equal(None)
    }
    it("recovers Float") {
      ValueReader.unpack[Float](67)(explicit.floatRecoveringGetter) must equal(Some(67.0F))
      ValueReader.unpack[Float]("10.87")(explicit.floatRecoveringGetter) must equal(Some(10.87F))
      ValueReader.unpack[Float](109L)(explicit.floatRecoveringGetter) must equal(Some(109.0F))
      ValueReader.unpack[Float]("x13")(explicit.floatRecoveringGetter) must equal(None)
    }
    it("recovers Boolean") {
      ValueReader.unpack[Boolean](67)(explicit.booleanRecoveringGetter) must equal(Some(true))
      ValueReader.unpack[Boolean]("")(explicit.booleanRecoveringGetter) must equal(Some(false))
      ValueReader.unpack[Boolean]("0")(explicit.booleanRecoveringGetter) must equal(Some(true))
      ValueReader.unpack[Boolean](0.0)(explicit.booleanRecoveringGetter) must equal(Some(false))
      ValueReader.unpack[Boolean](0.3)(explicit.booleanRecoveringGetter) must equal(Some(true))
      ValueReader.unpack[Boolean](0L)(explicit.booleanRecoveringGetter) must equal(Some(false))
    }
    it("recovers Date") {
      import java.util.Date
      // round to seconds
      val now = {
        val d = new Date
        d.setTime(d.getTime/1000 * 1000L)
        d
      }

      ValueReader.unpack[Date](now)(explicit.dateRecoveringGetter) must equal(Some(now))
      ValueReader.unpack[Date]((now.getTime/1000).intValue)(explicit.dateRecoveringGetter) must equal(Some(now))
      ValueReader.unpack[Date](now.getTime)(explicit.dateRecoveringGetter) must equal(Some(now))
    }
  }
}
