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

/** ValueReader is responsible for reading types from BSON values
  *
  * TODO: pf, link to companion
  * TODO: scala types
  */
@implicitNotFound(msg = "Cannot find ValueReader for ${T}")
trait ValueReader[+T] {
  def unpack(o: Any): Option[T]
}

/** ValueWriter is responsible for converting types to BSON values.
  * 
  * TODO: link to companion
  */
@implicitNotFound(msg = "Cannot find ValueWriter for ${T}")
trait ValueWriter[-T] {
  def pack(x: T): Option[Any]
}

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

object ValueReader {
  def apply[T](pf: PartialFunction[Any,T]): ValueReaderPf[T] = new ValueReaderPf[T](pf)

  implicit def defaultReader[T <: AnyRef](implicit m: Manifest[T]): ValueReader[T] =
    new ValueReader[T] {
      def unpack(o: Any): Option[T] =
        PartialFunction.condOpt(o) {
          case any: AnyRef if m.erasure isAssignableFrom any.getClass => any.asInstanceOf[T]
        }
    }
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
