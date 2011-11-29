package com.osinka.subset

import java.util.Date
import java.util.regex.Pattern
import annotation.implicitNotFound
import util.matching.Regex
import org.bson.types.{Symbol => BsonSymbol}
import com.mongodb.DBObject

@implicitNotFound(msg = "Cannot find Getter for ${T}")
trait Getter[+T] {
  def get(key: String, dbo: DBObject): Option[T]
}

@implicitNotFound(msg = "Cannot find Setter for ${T}")
trait Setter[-T] {
  def set(key: String, x: T, dbo: DBObject): Unit
}

object Getter {
  case class PfGetter[+T](val pf: PartialFunction[Any, T]) extends Getter[T] {
    override def get(key: String, dbo: DBObject): Option[T] =
      dbo.get(key) match {
        case null => None
        case x => PartialFunction.condOpt(x)(pf)
      }

    def orElse[B1 >: T](pf2: PartialFunction[Any,B1]): PfGetter[B1] = copy(pf = pf orElse pf2)
    def orElse[B1 >: T](g: PfGetter[B1]): PfGetter[B1] = orElse(g.pf)

    def andThen[R](pf2: PartialFunction[T,R]) =
      copy(pf = new PartialFunction[Any,R] {
          override def isDefinedAt(x: Any) = pf.isDefinedAt(x) && pf2.isDefinedAt(pf(x))
          override def apply(x: Any): R = pf2(pf(x))
        })

  }

  def apply[T](pf: PartialFunction[Any,T]) = new PfGetter[T](pf)
}

object Setter {
  class DefaultSetter[T] extends Setter[T] {
    override def set(key: String, x: T, dbo: DBObject) {
      dbo.put(key, x)
    }
  }

  def apply[T](sane: (T => Any)) =
    new Setter[T] {
      override def set(key: String, x: T, dbo: DBObject) {
        dbo.put(key, sane(x))
      }
    }
}

// Feel free to import to activate implicits:
object StrictPrimitivesSerializer extends StrictPrimitivesSerializer
object RecoveringPrimitivesSerializer extends RecoveringPrimitivesSerializer

// Lowest priority
trait DefaultPrimitiveSerializer {
  implicit def defaultSetter[T] = new Setter.DefaultSetter[T]
}

/**
 * Basic implicit getters and setters along with some explicit transformations
 */
trait BasePrimitivesSerializer extends DefaultPrimitiveSerializer {
  implicit val symbolSetter = Setter[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = Setter[Regex](r => r.pattern)
  
  implicit val stringGetter = Getter[String]({ case s: String => s})
  implicit val symbolGetter = Getter[Symbol]({
      case s: Symbol => s
      case s: BsonSymbol => Symbol(s.getSymbol)
    })
  implicit val booleanGetter = Getter[Boolean]({ case b: java.lang.Boolean => b.booleanValue })
  implicit val regexGetter = Getter[Regex]({
      case p: Pattern => new Regex(p.pattern)
      case r: Regex => r
    })
}

/**
 * Deserialize primitives "as is", without attempt to convert from other type
 */
trait StrictPrimitivesSerializer extends BasePrimitivesSerializer {
  implicit val intGetter = Getter[Int]({ case i: Int => i })
  implicit val longGetter = Getter[Long]({ case l: Long => l })
  implicit val byteGetter = Getter[Byte]({ case b: Byte => b })
  implicit val doubleGetter = Getter[Double]({ case d: Double => d })
  implicit val floatGetter = Getter[Float]({ case d: Double => d.floatValue })
  implicit val dateGetter = Getter[Date]({ case d: Date => d })
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

  implicit val intGetter = Getter[Int]({
      case b: Byte => b.intValue
      case i: Int => i
      case l: Long => l.intValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i })
  
  implicit val longGetter = Getter[Long]({
      case b: Byte => b.longValue
      case i: Int => i.longValue
      case l: Long => l
    }) orElse stringGetter.andThen({ case AsLong(l) => l })
  
  implicit val byteGetter = Getter[Byte]({
      case b: Byte => b
      case i: Int => i.byteValue
      case l: Long => l.byteValue
    }) orElse stringGetter.andThen({ case AsInt(i) => i.byteValue })

  implicit val doubleGetter = Getter[Double]({
      case d: Double => d
    }) orElse stringGetter.andThen({ case AsDouble(d) => d })
  implicit val floatGetter = Getter[Float]({
      case d: Double => d.floatValue
    }) orElse stringGetter.andThen({ case AsDouble(d) => d.floatValue })

  implicit val dateGetter = Getter[Date]({
      case d: Date => d
      case i: Int => new Date(i.longValue*1000L)
      case l: Long => new Date(l)
    })
}
