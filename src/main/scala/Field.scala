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

class Field[T](override val name: String)(implicit scope: Scope) extends Address with FieldConditions[T] {
  override def longName: String = (name :: scope.names).reverse mkString "."

  /**
   * Get "Index" field
   * 
   * Field[Int] is of much help to produce [Int] queries (in $special, $maxKey, $minKey, sort, index, etc.)
   */
  def int: Field[Int] = new Field[Int](name)(scope)

  /**
   * Get "Any" field
   * 
   * Field[Any] is of much help to insert custom objects or e.g. org.bson.types.{MaxKey, MinKey}
   */
  def any: Field[Any] = new Field[Any](name)(scope)

  // TODO: $elemMatch

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

  def apply(x: T)(implicit setter: ValueWriter[T]): Serializer = Serializer(_.write(name, x))

  def unapply(dbo: DBObject)(implicit getter: ValueReader[T]): Option[T] = dbo.read[T](name)
}

object Field {
  def apply[T](name: String)(implicit scope: Scope): Field[T] = new Field[T](name)(scope)
}