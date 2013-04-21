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
import Operator._

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

  protected val fieldAsQuery =
    (f: Field[_], op: Operator) => Query(QueryMutation.write(f, op.v)(ValueWriter.anyWriter))
}

object Project extends PipelineOperator("$project") {
  val Include = Operator[Int](1)
  val Exclude = Operator[Int](0)
  def toLower[T](f: Field[T]) = Operator("$toLower", f)
  def toUpper[T](f: Field[T]) = Operator("$toUpper", f)

  // default
  def all(fields: Field[_]*): DBObject =
    apply((Query.empty /: fields) { (q,t) => q && t === Include})

  // remap {stats: {pv: "$f1", p2: "$f2"}}
  def apply(query: Query): DBObject =
    gen(query)
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
  // Group operators: http://docs.mongodb.org/manual/reference/aggregation/#group-operators
  def Eq(f: Field[_]) = Operator(f)
  def AddToSet[A](f: Field[A]) = Operator("$addToSet", f)
  def First[A](f: Field[A]) = Operator("$first", f)
  def Last[A](f: Field[A]) = Operator("$last", f)
  def Max[A](f: Field[A]) = Operator("$max", f)
  def Min[A](f: Field[A]) = Operator("$min", f)
  def Avg[A](f: Field[A]) = Operator("$avg", f)
  def Push[A](f: Field[A]) = Operator("$push", f)
  def Sum(i: Int) = Operator("$sum", i)
  def Sum[A](f: Field[A]) = Operator("$sum", f)

  // id = field
  // id = field in subset
  def apply(id: Field[_], pairs: (Field[_], Operator)*) = {
    val l = (Document.DocumentId -> Eq(id)) +: pairs
    gen( (Query.empty /: l) { (q, t) => q && fieldAsQuery.tupled(t) } )
  }

  // id = document
  def apply(q: Query, pairs: (Field[_], Operator)*) =
    gen( (q /: pairs) { (q,t) => q && fieldAsQuery.tupled(t) } )
}

object Sort extends PipelineOperator("$sort") {
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})
}