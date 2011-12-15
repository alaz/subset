package com.osinka.subset

import com.mongodb.DBObject
import RichDBO._

class Field[T](val name: String)(implicit outer: Path) extends Path with FieldConditions[T] with Modifications[T] {
  override val path: List[String] = outer.path :+ name

  /**
   * Get "Index" field
   * 
   * Field[Int] is of much help to produce [Int] queries (in $special, $maxKey, $minKey, sort, index, etc.)
   */
  def int: Field[Int] = new Field[Int](name)

  /**
   * Get "Any" field
   * 
   * Field[Any] is of much help to insert custom objects or e.g. org.bson.types.{MaxKey, MinKey}
   */
  def any: Field[Any] = new Field[Any](name)

  // TODO: $elemMatch

  def ~[T2](f2: Field[T2]) = new Tuple2Subset[T,T2](this.name, f2.name)

  def apply(x: T)(implicit setter: ValueWriter[T]): Serializer = Serializer(_.write(name, x))

  def unapply(dbo: DBObject)(implicit getter: ValueReader[T]): Option[T] = dbo.read[T](name)
}

object Field {
  def apply[T](name: String)(implicit outer: Path): Field[T] = new Field[T](name)
}