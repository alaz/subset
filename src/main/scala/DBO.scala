package com.osinka.subset

import com.mongodb.{DBObject, BasicDBObjectBuilder}

object DBO {
  def empty = BasicDBObjectBuilder.start.get

  implicit def dboMethods(dbo: DBObject) =
    new AnyRef {
      def write(key: String, value: Any): DBObject = {
        dbo.put(key, value)
        dbo
      }
    }
}
