package com.osinka.subset

import com.mongodb.DBObject

trait Routines {
  import RichDBO._

  def unpackValue[T](o: Any)(implicit getter: ValueReader[T]) = getter.unpack(o)
  def packValue[T](x: T)(implicit setter: ValueWriter[T]): Option[Any] = setter.pack(x)
}
