package com.osinka.subset

import com.mongodb.DBObject

trait Scope {
  def names: List[String] = Nil
}

object Scope {
  val CollectionLevel = new Scope {}
}

class Field[T](val name: String)(implicit scope: Scope) {
  def longName: String = (name :: scope.names).reverse mkString "."

  def apply(x: T)(implicit setter: Setter[T]): (DBObject => DBObject) =
    (dbo: DBObject) => {
      setter.set(name, x, dbo)
      dbo
    }

  def unapply(dbo: DBObject)(implicit getter: Getter[T]): Option[T] =
    getter.get(name, dbo)

  def ~[T2](f2: Field[T2]) = new Tuple2Extractor[T,T2](this.name, f2.name)
}
