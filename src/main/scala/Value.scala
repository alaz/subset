package com.osinka.subset

import java.util.Date
import java.util.regex.Pattern
import annotation.implicitNotFound
import util.matching.Regex
import org.bson.types.{Symbol => BsonSymbol}

// ValueReader ?
@implicitNotFound(msg = "Cannot find transformation from BSON object to ${T}")
trait ValueDeserializer[+T] {
  // unpack ?
  def deserialize(o: Any): Option[T]
}

// ValueWriter ?
@implicitNotFound(msg = "Cannot find transformation from ${T} to BSON object")
trait ValueSerializer[-T] {
  // pack ?
  def serialize(x: T): Option[Any]
}

object ValueDeserializer {
  case class DefaultImpl[+T](val pf: PartialFunction[Any, T]) extends ValueDeserializer[T] {
    override def deserialize(o: Any): Option[T] = PartialFunction.condOpt(o)(pf)

    def orElse[B1 >: T](pf2: PartialFunction[Any,B1]): DefaultImpl[B1] = copy(pf = pf orElse pf2)
    def orElse[B1 >: T](g: DefaultImpl[B1]): DefaultImpl[B1] = orElse(g.pf)

    def andThen[R](pf2: PartialFunction[T,R]) =
      copy(pf = new PartialFunction[Any,R] {
          override def isDefinedAt(x: Any) = pf.isDefinedAt(x) && pf2.isDefinedAt(pf(x))
          override def apply(x: Any): R = pf2(pf(x))
        })

  }

  def apply[T](pf: PartialFunction[Any,T]) = new DefaultImpl[T](pf)
}

object ValueSerializer {
  def apply[T](sane: (T => Any)): ValueSerializer[T] =
    new ValueSerializer[T] {
      override def serialize(x: T): Option[Any] = Some(sane(x))
    }

  implicit def defaultSerializer[T]: ValueSerializer[T] =
    new ValueSerializer[T] {
      override def serialize(x: T): Option[Any] = Some(x)
    }
}

// Feel free to import to activate implicits:
object StrictPrimitivesSerializer extends StrictPrimitivesSerializer
object RecoveringPrimitivesSerializer extends RecoveringPrimitivesSerializer

// Lowest priority
trait DefaultPrimitiveSerializer {
//  implicit def defaultSerializer[T] = new DefaultSerializer[T]
}

/**
 * Basic implicit getters and setters along with some explicit transformations
 */
trait BasePrimitivesSerializer extends DefaultPrimitiveSerializer {
  implicit val symbolSetter = ValueSerializer[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = ValueSerializer[Regex](r => r.pattern)
  
  implicit val stringGetter = ValueDeserializer[String]({ case s: String => s})
  implicit val symbolGetter = ValueDeserializer[Symbol]({
      case s: Symbol => s
      case s: BsonSymbol => Symbol(s.getSymbol)
    })
  implicit val booleanGetter = ValueDeserializer[Boolean]({ case b: java.lang.Boolean => b.booleanValue })
  implicit val regexGetter = ValueDeserializer[Regex]({
      case p: Pattern => new Regex(p.pattern)
      case r: Regex => r
    })
}

/**
 * Deserialize primitives "as is", without attempt to convert from other type
 */
trait StrictPrimitivesSerializer extends BasePrimitivesSerializer {
  implicit val intGetter = ValueDeserializer[Int]({ case i: Int => i })
  implicit val longGetter = ValueDeserializer[Long]({ case l: Long => l })
  implicit val byteGetter = ValueDeserializer[Byte]({ case b: Byte => b })
  implicit val doubleGetter = ValueDeserializer[Double]({ case d: Double => d })
  implicit val floatGetter = ValueDeserializer[Float]({ case d: Double => d.floatValue })
  implicit val dateGetter = ValueDeserializer[Date]({ case d: Date => d })
}

/**
 * Deserialize primitives and try to convert from other type:
 * 
 * - tries to get Int, Long, Double, Byte from String
 * - tries to get DateTime (java.util.Date) from Int (as a number of seconds from the epoch)
 *   or Long (as a number of milliseconds from the epoch)
 */
trait RecoveringPrimitivesSerializer extends BasePrimitivesSerializer {
  import net.liftweb.util.BasicTypesHelpers.{AsInt,AsDouble,AsLong}

  implicit val intGetter = ValueDeserializer[Int]({
      case b: Byte => b.intValue
      case i: Int => i
      case l: Long => l.intValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i })
  
  implicit val longGetter = ValueDeserializer[Long]({
      case b: Byte => b.longValue
      case i: Int => i.longValue
      case l: Long => l
    }) orElse stringGetter.andThen({ case AsLong(l) => l })
  
  implicit val byteGetter = ValueDeserializer[Byte]({
      case b: Byte => b
      case i: Int => i.byteValue
      case l: Long => l.byteValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i.byteValue })

  implicit val doubleGetter = ValueDeserializer[Double]({
      case i: Int => i.doubleValue
      case l: Long => l.doubleValue
      case b: Byte => b.doubleValue
      case f: Float => f.doubleValue
      case d: Double => d
    }) orElse stringGetter.andThen({ case AsDouble(d) => d })
  implicit val floatGetter = ValueDeserializer[Float]({
      case i: Int => i.floatValue
      case l: Long => l.floatValue
      case b: Byte => b.floatValue
      case f: Float => f
      case d: Double => d.floatValue
    }) orElse stringGetter.andThen({ case AsDouble(d) => d.floatValue })

  implicit val dateGetter = ValueDeserializer[Date]({
      case d: Date => d
      case i: Int => new Date(i.longValue*1000L)
      case l: Long => new Date(l)
    })
}
