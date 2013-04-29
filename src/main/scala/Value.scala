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
import org.bson.types.{ObjectId, Binary, Symbol => BsonSymbol}
import com.mongodb.DBObject

import Mutation._

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
object ValueReader {
  import collection.JavaConverters._
  import org.bson.types.BasicBSONList

  def apply[T](pf: PartialFunction[Any,T]): ValueReaderPf[T] = new ValueReaderPf[T](pf)

  def unpack[T](o: Any)(implicit getter: ValueReader[T]) = getter.unpack(o)

  //
  // Default readers
  //

  implicit val booleanGetter = ValueReader[Boolean]({ case b: Boolean => b })
  implicit val intGetter = ValueReader[Int]({ case i: Int => i })
  implicit val shortGetter = ValueReader[Short]({ case i: Short => i })
  implicit val longGetter = ValueReader[Long]({ case l: Long => l })
  implicit val floatGetter = ValueReader[Float]({
      case d: Double => d.floatValue
      case f: Float => f
    })
  implicit val doubleGetter = ValueReader[Double]({ case d: Double => d })
  implicit val dateGetter = ValueReader[Date]({ case d: Date => d })

  implicit val objIdGetter = ValueReader[ObjectId]({ case objId: ObjectId => objId })
  implicit val dboGetter = ValueReader[DBObject]({ case dbo: DBObject => dbo })
  implicit val patternGetter = ValueReader[Pattern]({ case p: Pattern => p })
  implicit val stringGetter = ValueReader[String]({
    case s: String => s
    case s: BsonSymbol => s.getSymbol
    case oid: ObjectId => oid.toStringMongod
  })
  implicit val symbolGetter = ValueReader[Symbol]({
    case s: Symbol => s
    case s: BsonSymbol => Symbol(s.getSymbol)
  })
  implicit val regexGetter = ValueReader[Regex]({
    case p: Pattern => new Regex(p.pattern)
    case r: Regex => r
  })

  implicit def byteArrayGetter = ValueReader[Array[Byte]]({
      case b: Binary => b.getData
      case a: Array[Byte] => a
    })
  implicit def arrayGetter[T](implicit r: ValueReader[T], m: Manifest[T]) =
    ValueReader[Array[T]]({
      case a: Array[_] => a.asInstanceOf[Array[T]]
      case list: BasicBSONList => list.asScala flatMap {r.unpack _} toArray
    })

  implicit def optionGetter[T](implicit r: ValueReader[T]) =
    new ValueReader[Option[T]] {
      override def unpack(o: Any): Option[Option[T]] = Some(r.unpack(o))
    }
  implicit def listGetter[T](implicit r: ValueReader[T]) =
    ValueReader[List[T]]({
      case ar: Array[_] => ar flatMap {r.unpack _} toList
      case list: BasicBSONList => list.asScala flatMap {r.unpack _} toList
    })
  implicit def tuple2Getter[T1,T2](implicit r1: ValueReader[T1], r2: ValueReader[T2]) =
    new ValueReader[Tuple2[T1,T2]] {
      override def unpack(o: Any): Option[Tuple2[T1,T2]] =
        o match {
          case a: Array[_] if a.size == 2 =>
            for {v1 <- r1.unpack(a(0)); v2 <- r2.unpack(a(1))}
            yield (v1, v2)
        }
    }
  // TODO: ValueReader[Map[String,T]]
}

/** ValueWriter factory based on ordinary "sanitization" function.
  *
  * It contains default writer implicits as well.
  */
object ValueWriter {
  def apply[T](sane: (T => Any)): ValueWriter[T] =
    new ValueWriter[T] {
      override def pack(x: T): Option[Any] = Some(sane(x))
    }

  def pack[T](x: T)(implicit setter: ValueWriter[T]): Option[Any] = setter.pack(x)

  //
  // Default writers
  //
  val anyWriter = ValueWriter[Any](identity _)
  implicit val booleanSetter = ValueWriter[Boolean](identity _)
  implicit val intSetter = ValueWriter[Int](identity _)
  implicit val shortSetter = ValueWriter[Short](identity _)
  implicit val longSetter = ValueWriter[Long](identity _)
  implicit val floatSetter = ValueWriter[Float](identity _)
  implicit val doubleSetter = ValueWriter[Double](identity _)
  implicit val dateSetter = ValueWriter[Date](identity _)
  implicit val objIdSetter = ValueWriter[ObjectId](identity _)
  implicit val dboSetter = ValueWriter[DBObject](identity _)
  implicit val patternSetter = ValueWriter[Pattern](identity _)
  implicit val stringSetter = ValueWriter[String](identity _)

  implicit val symbolSetter = ValueWriter[Symbol](s => new BsonSymbol(s.name))
  implicit val regexSetter = ValueWriter[Regex](r => r.pattern)

  implicit val byteArraySetter = ValueWriter[Array[Byte]](new Binary(_))
  implicit val arraySetter = ValueWriter[Array[_]](identity _)

  implicit def optionSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Option[T]] {
      override def pack(x: Option[T]): Option[Any] = x flatMap { w.pack _}
    }
  implicit def seqSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[Traversable[T]] {
      override def pack(x: Traversable[T]): Option[Any] = Some( x flatMap {w.pack _} toArray )
    }
  implicit def fieldTupleSetter[T](implicit w: ValueWriter[T]) =
    new ValueWriter[(Field[T], T)] {
      override def pack(x: (Field[T], T)): Option[Any] =
        w.pack(x._2) map {v => writer(x._1.name, v)(anyWriter).get}
    }
  implicit def tuple2Setter[T1,T2](implicit w1: ValueWriter[T1], w2: ValueWriter[T2]) =
    new ValueWriter[Tuple2[T1,T2]] {
      override def pack(t: Tuple2[T1,T2]) =
        for {x1 <- w1.pack(t._1); x2 <- w2.pack(t._2)}
        yield Array(x1,x2)
    }
  // TODO: ValueWriter[Map[String,T]]
}

