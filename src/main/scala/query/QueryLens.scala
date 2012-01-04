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

import DBObjectLens._

/** The low level mechanism for constructing lenses dependant on Path.
  * 
  * Thus QueryLens is a function from Path to DBObjectLens. They compose well too.
  */
trait QueryLens extends (Path => DBObjectLens) {
  def ~(other: QueryLens): QueryLens =
    QueryLens( (p: Path) => this(p) ~ other(p) )
}

object QueryLens {
  def apply(f: Path => DBObjectLens): QueryLens =
    new QueryLens {
      def apply(p: Path): DBObjectLens = f(p)
    }

  implicit def fToQDBObjectLens(f: Path => DBObjectLens): QueryLens = apply(f)

  def wrap(p: Path,  ql: QueryLens): QueryLens =
    (scope: Path) => ql(scope + p)

  def write[T : ValueWriter](p: Path, x: T): QueryLens =
    (scope: Path) => writer[T]((scope + p).longName, x)

  def embed(s: String, ql: Traversable[QueryLens]): QueryLens = embed(Path(s), ql)
  def embed(p: Path, ql: Traversable[QueryLens]): QueryLens =
    (scope: Path) => writer(p.longName, ql map {_.apply(scope).get} toArray)

  def embed(s: String, ql: QueryLens): QueryLens = embed(Path(s), ql)
  def embed(p: Path, ql: QueryLens): QueryLens =
    (scope: Path) => writer(p.longName, ql(scope))
}
