package com.osinka.subset

import com.mongodb.DBObject
import RichDBO._

trait Serializer[-T] {
  def apply(x: T): (DBObject => DBObject)
}

object Serializer {
  implicit def serializer2value[T](s: Serializer[T]) =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some( s.apply(x)(empty) )
    }
}

trait Deserializer[+T] {
  def unapply(dbo: DBObject): Option[T]
}

object Deserializer {
  implicit def deserializer2value[T](d: Deserializer[T]) =
    new ValueReader[T] {
      override def unpack(o: Any): Option[T] =
        o match {
          case dbo: DBObject => d.unapply(dbo)
          case _ => None
        }
    }
}