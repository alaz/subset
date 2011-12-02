package com.osinka.subset

import com.mongodb.DBObject

trait Routines {
  import RichDBO._

  def unpackValue[T](o: Any)(implicit getter: ValueDeserializer[T]) = getter.deserialize(o)
  def packValue[T](x: T)(implicit setter: ValueSerializer[T]): Option[Any] = setter.serialize(x)
}
