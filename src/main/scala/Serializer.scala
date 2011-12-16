package com.osinka.subset

import com.mongodb.{DBObject,BasicDBObjectBuilder}

trait Serializer {
  def write: DBObject => DBObject

  def ~(other: Serializer) = Serializer(write andThen other.write)

  lazy val get = write(BasicDBObjectBuilder.start.get)

  protected def prefixString: String = "Serializer"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Serializer => other.get == get }

  override def hashCode: Int = get.hashCode

  override def toString: String = prefixString+get
}

object Serializer {
  private class DefaultSerializer(override val write: DBObject => DBObject) extends Serializer

  def read[T](key: String, dbo: DBObject)(implicit reader: ValueReader[T]): Option[T] =
    Option(dbo.get(key)) flatMap {reader.unpack(_)}

  def apply(f: DBObject => DBObject): Serializer = new DefaultSerializer(f)

  def writer[T](key: String, x: T)(implicit w: ValueWriter[T]): Serializer = new DefaultSerializer( (dbo: DBObject) => {
      w.pack(x) foreach {dbo.put(key, _)}
      dbo
    })

  implicit val serializerWriter = ValueWriter[Serializer](_.get)
}
