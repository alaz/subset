package com.osinka.subset

import com.mongodb.{DBObject,BasicDBObjectBuilder}
import Implicits._

private[subset] object Operations {
  val SET        = "$set"
  val UNSET      = "$unset"
  val INC        = "$inc"
  val PUSH       = "$push"
  val PUSH_ALL   = "$pushAll"
  val ADD_TO_SET = "$addToSet"
  val POP        = "$pop"
  val PULL       = "$pull"
  val PULL_ALL   = "$pullAll"
  val RENAME     = "$rename"
  val BIT        = "$bit"
}

import Operations._

trait Modifications[T] extends Path {
  private def op[B](op: String, x: B)(implicit writer: ValueWriter[B]) = Update(op -> ((path.last -> x): Serializer))

  def set(x: T)(implicit writer: ValueWriter[T]) = op(SET, x)
  def inc(x: T)(implicit writer: ValueWriter[T]) = op(INC, x)
  def unset(x: T)(implicit writer: ValueWriter[Int]) = op(UNSET, 1)
  def push(x: T)(implicit writer: ValueWriter[T]) = op(PUSH, x)
  def push(seq: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]): Update = pushAll(seq)
  def pushAll(seq: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = op(PUSH_ALL, seq)
  def addToSet(x: T)(implicit writer: ValueWriter[T]) = op(ADD_TO_SET, x)
  def addToSet(seq: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = op(ADD_TO_SET, "$each" -> seq)
  def pop(i: Int)(implicit writer: ValueWriter[Int]) = op(POP, i)
  def pull(x: T)(implicit writer: ValueWriter[T]) = op(PULL, x)
  def pull(q: Query) = op(PULL, q)
  def pull(seq: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]): Update = pullAll(seq)
  def pullAll(seq: Traversable[T])(implicit writer: ValueWriter[Traversable[T]]) = op(PULL_ALL, seq)
  def rename(newName: String)(implicit writer: ValueWriter[String]) = op(RENAME, newName)
  // TOOD: $bit
  // TODO: positional $
}

object Update {
  def apply(t: (String, Serializer)) = new Update(Map(t))

  implicit val updateWriter = ValueWriter[Update](_.get)
}

case class Update(val ops: Map[String,Serializer]) {
  def get: DBObject =
    (BasicDBObjectBuilder.start /: ops) {(builder, kv) => builder.append(kv._1, kv._2.get)} get

  def ~(other: Update) = {
    def mergeMaps(ms: Map[String,Serializer]*)(f: (Serializer, Serializer) => Serializer) =
      (Map[String,Serializer]() /: ms.flatten) { (m, kv) =>
        m + (if (m contains kv._1) kv._1 -> f(m(kv._1), kv._2)
             else kv)
      }
    
    copy(ops = mergeMaps(ops, other.ops) { _ ~ _ })
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Update => ops == other.ops
      case _ => false
    }

  override def hashCode: Int = ops.hashCode

  override def toString = "Update"+get
}