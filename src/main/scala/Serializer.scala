package com.osinka.subset

import com.mongodb.DBObject
import RichDBO._

trait Serializer {
  def write: (DBObject => DBObject)

  def ~(other: Serializer) = Serializer(write andThen other.write)
}

object Serializer {
  class DefaultSerializer(f: DBObject => DBObject) extends Serializer {
    override def write: DBObject => DBObject = f
  }

  def apply(f: DBObject => DBObject): Serializer = new DefaultSerializer(f)
}
