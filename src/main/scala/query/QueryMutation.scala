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

import Mutation._

/** The low level mechanism for constructing mutations dependant on Path.
  *
  * Thus QueryMutation is a function from Path to Mutation. They compose well too.
  */
trait QueryMutation extends (Path => Mutation) {
  def ~(other: QueryMutation): QueryMutation =
    QueryMutation( (p: Path) => this(p) ~ other(p) )
}

object QueryMutation {
  def apply(f: Path => Mutation): QueryMutation =
    new QueryMutation {
      def apply(p: Path): Mutation = f(p)
    }

  implicit def fToQMutation(f: Path => Mutation): QueryMutation = apply(f)

  def noop: QueryMutation =
    (_: Path) => Mutation.noop

  def wrap(p: Path,  ql: QueryMutation): QueryMutation =
    (scope: Path) => ql(scope + p)

  def write[T : ValueWriter](p: Path, x: T): QueryMutation =
    (scope: Path) => writer[T]((scope + p).longName, x)

  def embed(s: String, ql: Traversable[QueryMutation]): QueryMutation = embed(Path(s), ql)
  def embed(p: Path, ql: Traversable[QueryMutation]): QueryMutation =
    (scope: Path) => writer(p.longName, ql map {_.apply(scope).get} toArray)

  def embed(s: String, ql: QueryMutation): QueryMutation = embed(Path(s), ql)
  def embed(p: Path, ql: QueryMutation): QueryMutation =
    (scope: Path) => writer(p.longName, ql(scope))
}
