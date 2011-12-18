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
package query

import Lens._

/** The low level mechanism for constructing lenses dependant on Path.
  * 
  * Thus QueryLens is a function from Path to Lens. They compose well too.
  */
trait QueryLens extends (Path => Lens) {
  def ~(other: QueryLens): QueryLens =
    QueryLens( (p: Path) => this(p) ~ other(p) )
}

object QueryLens {
  def apply(f: Path => Lens): QueryLens =
    new QueryLens {
      def apply(p: Path): Lens = f(p)
    }

  implicit def fToQLens(f: Path => Lens): QueryLens = apply(f)

  def relative[T : ValueWriter](p: Path, x: T): QueryLens =
    (scope: Path) => writer(p.relativeTo(scope).longName, x)

  def positional[T : ValueWriter](p: Path, x: T): QueryLens =
    (scope: Path) => writer(p.positionIn(scope).longName, x)

  def embed(s: String, ql: Traversable[QueryLens]): QueryLens =
    (scope: Path) => writer(s, ql map {_.apply(scope).get} toArray)

  def embed(s: String, ql: QueryLens): QueryLens =
    (scope: Path) => writer(s, ql(scope))
}
