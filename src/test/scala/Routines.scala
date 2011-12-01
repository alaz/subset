package com.osinka.subset

import com.mongodb.DBObject

trait Routines {
  import DBO._

  def applyGetter[T](f: String, dbo: DBObject)(implicit getter: Getter[T]) = getter.get(f, dbo)

  def applySetter[T](f: String, x: T, dbo: DBObject = empty)(implicit setter: Setter[T]): DBObject =
    setter.set(f, x, dbo)
}
