package com.osinka.subset

import java.util.Date
import java.util.regex.Pattern
import annotation.implicitNotFound
import util.matching.Regex
import org.bson.types.{Symbol => BsonSymbol}
import RichDBO._

@implicitNotFound(msg = "Cannot find reader from BSON object to ${T}")
trait ValueReader[+T] {
  def unpack(o: Any): Option[T]
}

@implicitNotFound(msg = "Cannot find writer from ${T} to BSON object")
trait ValueWriter[-T] {
  def pack(x: T): Option[Any]
}

case class ValueReaderPf[+T](val pf: PartialFunction[Any, T]) extends ValueReader[T] {
  override def unpack(o: Any): Option[T] = PartialFunction.condOpt(o)(pf)

  def orElse[B1 >: T](pf2: PartialFunction[Any,B1]): ValueReaderPf[B1] = copy(pf = pf orElse pf2)
  def orElse[B1 >: T](g: ValueReaderPf[B1]): ValueReaderPf[B1] = orElse(g.pf)

  def andThen[R](pf2: PartialFunction[T,R]) =
    copy(pf = new PartialFunction[Any,R] {
        override def isDefinedAt(x: Any) = pf.isDefinedAt(x) && pf2.isDefinedAt(pf(x))
        override def apply(x: Any): R = pf2(pf(x))
      })

}

object ValueReader {
  def apply[T](pf: PartialFunction[Any,T]): ValueReaderPf[T] = new ValueReaderPf[T](pf)
}

object ValueWriter {
  def apply[T](sane: (T => Any)): ValueWriter[T] =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some(sane(x))
    }

  implicit def defaultWriter[T]: ValueWriter[T] =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some(x)
    }
}

// Feel free to import to activate implicits:
object StrictValuePacking extends StrictValuePacking
object RecoveringValuePacking extends RecoveringValuePacking

// Lowest priority
trait LowPriorityValuePacking

/**
 * Basic implicit getters and setters along with some explicit transformations
 */
trait BaseValuePacking extends LowPriorityValuePacking {
  implicit val symbolSetter = ValueWriter[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = ValueWriter[Regex](r => r.pattern)
  
  implicit val stringGetter = ValueReader[String]({ case s: String => s})
  implicit val symbolGetter = ValueReader[Symbol]({
      case s: Symbol => s
      case s: BsonSymbol => Symbol(s.getSymbol)
    })
  implicit val booleanGetter = ValueReader[Boolean]({ case b: java.lang.Boolean => b.booleanValue })
  implicit val regexGetter = ValueReader[Regex]({
      case p: Pattern => new Regex(p.pattern)
      case r: Regex => r
    })

  implicit def optionSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Option[T]] {
      override def pack(x: Option[T]): Option[Any] = x flatMap { w.pack _}
    }
  implicit def seqSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Seq[T]] {
      override def pack(x: Seq[T]): Option[Any] = Some( x flatMap {w.pack _} toArray )
    }
  implicit def tupleSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Tuple2[String,T]] {
      override def pack(x: Tuple2[String,T]): Option[Any] =
        w.pack(x._2) map { empty.write(x._1, _)(ValueWriter.defaultWriter[Any]).get }
    }
  // TODO: ValueWriter[Either[_,T]]
}

/**
 * unpack primitives "as is", without attempt to convert from other type
 */
trait StrictValuePacking extends BaseValuePacking {
  implicit val intGetter = ValueReader[Int]({ case i: Int => i })
  implicit val longGetter = ValueReader[Long]({ case l: Long => l })
  implicit val byteGetter = ValueReader[Byte]({ case b: Byte => b })
  implicit val doubleGetter = ValueReader[Double]({ case d: Double => d })
  implicit val floatGetter = ValueReader[Float]({ case d: Double => d.floatValue })
  implicit val dateGetter = ValueReader[Date]({ case d: Date => d })
}

/**
 * unpack primitives and try to convert from other type:
 * 
 * - tries to get Int, Long, Double, Byte from String
 * - tries to get DateTime (java.util.Date) from Int (as a number of seconds from the epoch)
 *   or Long (as a number of milliseconds from the epoch)
 */
trait RecoveringValuePacking extends BaseValuePacking {
  import net.liftweb.util.BasicTypesHelpers.{AsInt,AsDouble,AsLong}

  implicit val intGetter = ValueReader[Int]({
      case b: Byte => b.intValue
      case i: Int => i
      case l: Long => l.intValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i })
  
  implicit val longGetter = ValueReader[Long]({
      case b: Byte => b.longValue
      case i: Int => i.longValue
      case l: Long => l
    }) orElse stringGetter.andThen({ case AsLong(l) => l })
  
  implicit val byteGetter = ValueReader[Byte]({
      case b: Byte => b
      case i: Int => i.byteValue
      case l: Long => l.byteValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i.byteValue })

  implicit val doubleGetter = ValueReader[Double]({
      case i: Int => i.doubleValue
      case l: Long => l.doubleValue
      case b: Byte => b.doubleValue
      case f: Float => f.doubleValue
      case d: Double => d
    }) orElse stringGetter.andThen({ case AsDouble(d) => d })
  implicit val floatGetter = ValueReader[Float]({
      case i: Int => i.floatValue
      case l: Long => l.floatValue
      case b: Byte => b.floatValue
      case f: Float => f
      case d: Double => d.floatValue
    }) orElse stringGetter.andThen({ case AsDouble(d) => d.floatValue })

  implicit val dateGetter = ValueReader[Date]({
      case d: Date => d
      case i: Int => new Date(i.longValue*1000L)
      case l: Long => new Date(l)
    })
}
