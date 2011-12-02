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

trait Address {
  def name: String
  def longName: String
}

class Field[T](override val name: String)(implicit scope: Scope) extends Address /*extends Conditions[T]*/ {
  override def longName: String = (name :: scope.names).reverse mkString "."

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

//  def ==?(x: T)(implicit setter: unpackr[T]) = Condition[T](this, apply(x)(setter))
//  def >?(x: T)(implicit setter: unpackr[T]) = Condition[T](this, QueryOp.gt(apply(x)(setter)))

  // TODO: apply
  def serializer[T](implicit setter: ValueWriter[T]): Serializer[T] =
    new Serializer[T] {
      def apply(x: T): (DBObject => DBObject) = (dbo: DBObject) => dbo.write(name, x)
    }

  // TODO: unapply
  def deserializer[T](implicit getter: ValueReader[T]): Deserializer[T] =
    new Deserializer[T] {
      def unapply(dbo: DBObject): Option[T] = dbo.read[T](name)
    }
}

object Field {
  def apply[T](name: String)(implicit scope: Scope): Field[T] = new Field[T](name)(scope)
}