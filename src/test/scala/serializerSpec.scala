package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.bson.types.{Symbol => BsonSymbol}
import com.mongodb.{DBObject, BasicDBObject, BasicDBObjectBuilder}

@RunWith(classOf[JUnitRunner])
class serializerSpec extends Spec with MustMatchers with MongoMatchers {
  describe("Base primitives serializer") {
    val explicit = new BasePrimitivesSerializer {}

    it("must set") {
      applySetter("f", "val")(explicit.defaultSetter[String]) must containKeyValue("f" -> "val")
      applySetter("f", 10) must containKeyValue("f" -> new java.lang.Integer(10))
      applySetter("f", 'Sym) must containKeyValue("f" -> new BsonSymbol("Sym"))
    }
    it("must get") {
      val dbo = BasicDBObjectBuilder.start("s", "string").add("sym", new BsonSymbol("Sym")).get
      applyGetter[String]("s", dbo) must equal(Some("string"))
      applyGetter[Symbol]("s", dbo) must equal(None)
      applyGetter[Symbol]("sym", dbo) must equal(Some('Sym))
      applyGetter[String]("sym", dbo) must equal(None)
    }
  }

  def applyGetter[T](f: String, dbo: DBObject)(implicit getter: Getter[T]) = getter.get(f, dbo)

  def applySetter[T](f: String, x: T, dbo: DBObject = new BasicDBObject)(implicit setter: Setter[T]): DBObject = {
    setter.set(f, x, dbo)
    dbo
  }
}
