/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.osinka.subset

import annotation.implicitNotFound
import java.util.Date
import java.util.regex.Pattern
import util.matching.Regex
import org.bson.types.{ObjectId, Symbol => BsonSymbol}
import com.mongodb.DBObject

import DBObjectLens._

/** ValueReader is responsible for reading types from BSON values
  */
@implicitNotFound(msg = "Cannot find ValueReader for ${T}")
trait ValueReader[+T] {
  def unpack(o: Any): Option[T]
}

/** ValueWriter is responsible for converting types to BSON values.
  */
@implicitNotFound(msg = "Cannot find ValueWriter for ${T}")
trait ValueWriter[-T] {
  def pack(x: T): Option[Any]
}

/** Most typical implementation of [[com.osinka.subset.ValueReader]]
  *
  * This class supports not only `orElse` (like `PartialFunction`), but
  * `andThen` as well.
  */
case class ValueReaderPf[+T](pf: PartialFunction[Any, T]) extends ValueReader[T] {
  override def unpack(o: Any): Option[T] = PartialFunction.condOpt(o)(pf)

  def orElse[B1 >: T](pf2: PartialFunction[Any,B1]): ValueReaderPf[B1] = copy(pf = pf orElse pf2)
  def orElse[B1 >: T](g: ValueReaderPf[B1]): ValueReaderPf[B1] = orElse(g.pf)

  def andThen[R](pf2: PartialFunction[T,R]) =
    copy(pf = new PartialFunction[Any,R] {
        override def isDefinedAt(x: Any) = pf.isDefinedAt(x) && pf2.isDefinedAt(pf(x))
        override def apply(x: Any): R = pf2(pf(x))
      })

}

/** ValueReader factory based on `PartialFunction[Any,T]`.
  *
  * It contains default reader implicits as well.
  */
object ValueReader extends LowPriorityReaders {
  def apply[T](pf: PartialFunction[Any,T]): ValueReaderPf[T] = new ValueReaderPf[T](pf)

  //
  // Default readers
  //

  implicit val booleanGetter = ValueReader[Boolean]({ case b: Boolean => b })
  implicit val intGetter = ValueReader[Int]({ case i: Int => i })
  implicit val longGetter = ValueReader[Long]({ case l: Long => l })
  implicit val doubleGetter = ValueReader[Double]({ case d: Double => d })
  implicit val dateGetter = ValueReader[Date]({ case d: Date => d })

  implicit val dboGetter = ValueReader[DBObject]({ case dbo: DBObject => dbo })
  implicit val stringGetter = ValueReader[String]({
    case s: String => s
    case s: BsonSymbol => s.getSymbol
    case oid: ObjectId => oid.toString
  })
  implicit val symbolGetter = ValueReader[Symbol]({
    case s: Symbol => s
    case s: BsonSymbol => Symbol(s.getSymbol)
  })
  implicit val regexGetter = ValueReader[Regex]({
    case p: Pattern => new Regex(p.pattern)
    case r: Regex => r
  })

  implicit def optionGetter[T](implicit r: ValueReader[T]) =
    new ValueReader[Option[T]] {
      override def unpack(o: Any): Option[Option[T]] = Some(r.unpack(o))
    }
  implicit def listGetter[T](implicit r: ValueReader[T]) = {
    import collection.JavaConversions._
    import org.bson.types.BasicBSONList

    ValueReader[List[T]]({
      case ar: Array[_] => ar flatMap {r.unpack _} toList
      case list: BasicBSONList => list flatMap {r.unpack _} toList
    })
  }
  // TODO: ValueReader[Map[String,T]]
}

trait LowPriorityReaders {
  implicit def defaultReader[T <: AnyRef](implicit m: Manifest[T]): ValueReader[T] =
    new ValueReader[T] {
      def unpack(o: Any): Option[T] =
        PartialFunction.condOpt(o) {
          case any: AnyRef if m.erasure isAssignableFrom any.getClass => any.asInstanceOf[T]
        }
    }
}

/** ValueWriter factory based on ordinary "sanitization" function.
  * 
  * It contains default writer implicits as well.
  */
object ValueWriter extends LowPriorityWriters {
  def apply[T](sane: (T => Any)): ValueWriter[T] =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some(sane(x))
    }

  //
  // Default writers
  //

  implicit val symbolSetter = ValueWriter[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = ValueWriter[Regex](r => r.pattern)

  implicit def optionSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Option[T]] {
      override def pack(x: Option[T]): Option[Any] = x flatMap { w.pack _}
    }
  implicit def seqSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Traversable[T]] {
      override def pack(x: Traversable[T]): Option[Any] = Some( x flatMap {w.pack _} toArray )
    }
  implicit def tupleSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Tuple2[String,T]] {
      override def pack(x: Tuple2[String,T]): Option[Any] =
        w.pack(x._2) map {v => writer(x._1, v)(defaultWriter[Any]).get}
    }
  // TODO: ValueWriter[Map[String,T]]
}

trait LowPriorityWriters {
  implicit def defaultWriter[T]: ValueWriter[T] =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some(x)
    }
}
