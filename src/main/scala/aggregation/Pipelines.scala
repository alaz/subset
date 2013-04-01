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
package aggregation

import com.mongodb.DBObject
import query.QueryMutation

/**
 * http://docs.mongodb.org/ecosystem/tutorial/use-aggregation-framework-with-java-driver/
 *
 * aggregate(
 *   Match(type === "airfare"),
 *   Project(department, amount),
 *   Group(department, average -> Group.Avg(amount))
 * )
 */

class PipelineOperator(val method: String) {
  protected def gen[A : ValueWriter](contents: A): DBObject = Mutation.writer(method, contents)
}

object Project extends PipelineOperator("$project") {
  // default
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  // include / exclude
  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})

  // remap {stats: {pv: "$f1", p2: "$f2"}}
  def apply(query: Query): DBObject =
    gen(query)

  // TODO: $toUpper
}

object Match extends PipelineOperator("$match") {
  def apply(q: Query): DBObject = gen(q)
}

object Limit extends PipelineOperator("$limit") {
  def apply(n: Int): DBObject = gen(n)
}

object Skip extends PipelineOperator("$skip") {
  def apply(n: Int): DBObject = gen(n)
}

object Unwind extends PipelineOperator("$unwind") {
  def apply[A <: Traversable[_]](f: Field[A]): DBObject = gen(f.projection)
}

object Group extends PipelineOperator("$group") {
  case class Op(val v: Any)
  object Op {
    def apply[T : ValueWriter](f: String, v: T): Op = new Op(Mutation.writer(f, v) : DBObject)
  }

  def Eq(f: Field[_]) = Op( ValueWriter.pack(f.projection).get )
  def AddToSet[A](f: Field[A]) = Op("$addToSet", f.projection)
  def First[A](f: Field[A]) = Op("$first", f.projection)
  def Last[A](f: Field[A]) = Op("$last", f.projection)
  def Max[A](f: Field[A]) = Op("$max", f.projection)
  def Min[A](f: Field[A]) = Op("$min", f.projection)
  def Avg[A](f: Field[A]) = Op("$avg", f.projection)
  def Push[A](f: Field[A]) = Op("$push", f.projection)
  def Sum(i: Int) = Op("$sum", i)
  def Sum[A](f: Field[A]) = Op("$sum", f.projection)

  // id = field
  // id = field in subset
  def apply(id: Field[_], pairs: (Field[_], Op)*) = {
    val l = (Document.DocumentId -> Eq(id)) +: pairs
    gen( (Query.empty /: l) { (q, t) => q && fieldAsQuery.tupled(t) } )
  }

  // id = document
  def apply(q: Query, pairs: (Field[_], Op)*) =
    gen( (q /: pairs) { (q,t) => q && fieldAsQuery.tupled(t) } )

  private val fieldAsQuery =
    (f: Field[_], op: Op) => Query(QueryMutation.write(f, op.v)(ValueWriter.anyWriter))
}

object Sort extends PipelineOperator("$sort") {
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})
}