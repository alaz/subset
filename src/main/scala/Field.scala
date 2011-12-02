package com.osinka.subset

import com.mongodb.DBObject
import RichDBO._

trait Scope {
  def names: List[String] = Nil
}

object Scope {
  val CollectionLevel = new Scope {}

  implicit def slistToScope(flist: Seq[String]): Scope =
    new Scope {
      override val names: List[String] = flist.toList reverse
    }
}

class Field[T](val name: String)(implicit scope: Scope) /*extends Conditions[T]*/ {
  def longName: String = (name :: scope.names).reverse mkString "."

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

//  def ==?(x: T)(implicit setter: Deserializer[T]) = Condition[T](this, apply(x)(setter))
//  def >?(x: T)(implicit setter: Deserializer[T]) = Condition[T](this, QueryOp.gt(apply(x)(setter)))
}

object Field {
  def apply[T](name: String)(implicit scope: Scope): Field[T] = new Field[T](name)(scope)

  implicit def serializer[T](f: Field[T])(implicit setter: ValueSerializer[T]): Serializer[T] =
    new Serializer[T] {
      def apply(x: T): (DBObject => DBObject) =
        (dbo: DBObject) => dbo.write(f.name, x)
    }

  implicit def deserializer[T](f: Field[T])(implicit getter: ValueDeserializer[T]): Deserializer[T] =
    new Deserializer[T] {
      def unapply(dbo: DBObject): Option[T] =
        dbo.read[T](f.name)
    }
}