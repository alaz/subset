package com.osinka.subset

import com.mongodb.{DBObject, BasicDBObjectBuilder}

class RichDBO(val dbo: DBObject) {
  def write[T](key: String, x: T)(implicit serializer: ValueSerializer[T]): RichDBO = {
    serializer.serialize(x) foreach { dbo.put(key,_) }
    this
  }

  def read[T](key: String)(implicit deserializer: ValueDeserializer[T]): Option[T] =
    Option(dbo.get(key)) flatMap {deserializer.deserialize(_)}

  override def equals(o: Any): Boolean =
    o match {
      case other: RichDBO => dbo == other.dbo
      case _ => false
    }

  override def hashCode: Int = dbo.hashCode

  override def toString: String = "RichDBO ["+dbo+"]"
}

object RichDBO {
  def empty = new RichDBO(BasicDBObjectBuilder.start.get)

  implicit def richFromDBO(dbo: DBObject) = new RichDBO(dbo)

  implicit def richToDBO(r: RichDBO) = r.dbo
}
