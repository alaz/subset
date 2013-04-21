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

  protected val fieldAsQuery =
    (f: Field[_], op: Operator) => Query(QueryMutation.write(f, op.v)(ValueWriter.anyWriter))
}

/** `\$project`

  * == Selecting the fields ==
  * Select all the given fields:
  *
  * {{{
  * Project.all(field1, field2, field3)
  * }}}
  *
  * or select which fields to include/exclude:
  *
  * {{{
  * Project(field1 === Project.Include && field2 === Project.Exclude)
  * }}}
  *
  * == Compute values ==
  * {{{
  * Project(field1 === Project.Add(field2, 2))
  * }}}
  *
  * == Remap onto new document ==
  * {{{
  * Project(docField build {doc =>
  *   doc.field === sourceField &&
  *   doc.another === Project.DayOfYear(timestampField)
  * })
  * }}}
  *
  * @see [[http://docs.mongodb.org/manual/reference/aggregation/project/#stage._S_project \$project reference]]
  */
object Project extends PipelineOperator("$project") {
  val Include = Operator[Int](1)
  val Exclude = Operator[Int](0)

  // Project operators: http://docs.mongodb.org/manual/reference/aggregation/#aggregation-operators

  def And(args: Operator.Val*) = Operator("$and", args:_*)
  def Or(args: Operator.Val*) = Operator("$or", args:_*)
  def Not(arg: Operator.Val) = Operator("$not", arg)

  def Cmp(arg1: Operator.Val, arg2: Operator.Val) = Operator("$cmp", arg1, arg2)
  def Eq(arg1: Operator.Val, arg2: Operator.Val) = Operator("$eq", arg1, arg2)
  def Gt(arg1: Operator.Val, arg2: Operator.Val) = Operator("$gt", arg1, arg2)
  def Gte(arg1: Operator.Val, arg2: Operator.Val) = Operator("$gte", arg1, arg2)
  def Lt(arg1: Operator.Val, arg2: Operator.Val) = Operator("$lt", arg1, arg2)
  def Lte(arg1: Operator.Val, arg2: Operator.Val) = Operator("$lte", arg1, arg2)
  def Ne(arg1: Operator.Val, arg2: Operator.Val) = Operator("$ne", arg1, arg2)

  def Add(args: Operator.Val*) = Operator("$add", args:_*)
  def Divide(arg1: Operator.Val, arg2: Operator.Val) = Operator("$divide", arg1, arg2)
  def Mod(arg1: Operator.Val, arg2: Operator.Val) = Operator("$mod", arg1, arg2)
  def Multiply(args: Operator.Val*) = Operator("$multiply", args:_*)
  def Subtract(arg1: Operator.Val, arg2: Operator.Val) = Operator("$subtract", arg1, arg2)

  def Concat(args: Operator.Val*) = Operator("$concat", args:_*)
  def Strcasecmp(arg1: Operator.Val, arg2: Operator.Val) = Operator("$strcasecmp", arg1, arg2)
  def Substr(arg1: Operator.Val, arg2: Operator.Val, arg3: Operator.Val) = Operator("$substr", arg1, arg2, arg3)
  def ToLower(f: Field[_]) = Operator("$toLower", f)
  def ToUpper(f: Field[_]) = Operator("$toUpper", f)

  def DayOfYear(arg: Operator.Val) = Operator("$dayOfYear", arg)
  def DayOfMonth(arg: Operator.Val) = Operator("$dayOfMonth", arg)
  def DayOfWeek(arg: Operator.Val) = Operator("$dayOfWeek", arg)
  def Year(arg: Operator.Val) = Operator("$year", arg)
  def Month(arg: Operator.Val) = Operator("$month", arg)
  def Week(arg: Operator.Val) = Operator("$week", arg)
  def Hour(arg: Operator.Val) = Operator("$hour", arg)
  def Minute(arg: Operator.Val) = Operator("$minute", arg)
  def Second(arg: Operator.Val) = Operator("$second", arg)
  def Millisecond(arg: Operator.Val) = Operator("$millisecond", arg)

  def Cond(arg1: Operator.Val, arg2: Operator.Val, arg3: Operator.Val) = Operator("$cond", arg1, arg2, arg3)
  def IfNull(arg1: Operator.Val, arg2: Operator.Val) = Operator("$ifNull", arg1, arg2)

  // default
  def all(fields: Field[_]*): DBObject =
    apply((Query.empty /: fields) { (q,t) => q && t === Include})

  // remap {stats: {pv: "$f1", p2: "$f2"}}
  def apply(query: Query): DBObject =
    gen(query)
}

/** `\$match`
  */
object Match extends PipelineOperator("$match") {
  def apply(q: Query): DBObject = gen(q)
}

/** `\$limit`
  */
object Limit extends PipelineOperator("$limit") {
  def apply(n: Int): DBObject = gen(n)
}

/** `\$skip`
  */
object Skip extends PipelineOperator("$skip") {
  def apply(n: Int): DBObject = gen(n)
}

/** `\$unwind`
  */
object Unwind extends PipelineOperator("$unwind") {
  def apply[A <: Traversable[_]](f: Field[A]): DBObject = gen(f.projection)
}

/** `\$group`
  *
  * The first `Group` argument is the specification of identifier, it may be
  * a single field or a subdocument (declared as a `Query`). Other
  * arguments define grouping methods
  *
  * == Field identifier ==
  * {{{
  * Group(author, docsPerAuthor -> Group.Add(1))
  * }}}
  *
  * will create
  *
  * {{{
  * { \$group: {
  *   _id: "\$author",
  *   docsPerAuthor: {"\$add": 1} }
  * }}}
  *
  * When an identifier is based on a document field,
  *
  * {{{
  * Group(state in DocumentId, avgCityPop -> Group.Avg(population))
  * }}}
  *
  * will result in
  *
  * {{{
  * { \$group: {
  *   _id: "$_id.state",
  *   avgCityPop: {"\$avg": "\$pop"} }
  * }}}
  *
  * == Document identifier ==
  * {{{
  * Group(state === state && city === city, pop -> Group.Sum(pop))
  * }}}
  *
  * will result in
  *
  * {{{
  * { \$group: {_id: {state: "\$state", city: "\$city"},
  *              pop: {"\$sum": "\$pop"} } }
  * }}}
  *
  * @see [[http://docs.mongodb.org/manual/reference/aggregation/group/#stage._S_group \$group reference]]
  */
object Group extends PipelineOperator("$group") {
  // Group operators: http://docs.mongodb.org/manual/reference/aggregation/#group-operators
  def Eq(f: Field[_]) = Operator[String](f.projection)
  def AddToSet[A](f: Field[A]) = Operator("$addToSet", f)
  def First[A](f: Field[A]) = Operator("$first", f)
  def Last[A](f: Field[A]) = Operator("$last", f)
  def Max[A](f: Field[A]) = Operator("$max", f)
  def Min[A](f: Field[A]) = Operator("$min", f)
  def Avg[A](f: Field[A]) = Operator("$avg", f)
  def Push[A](f: Field[A]) = Operator("$push", f)
  def Sum(arg: Operator.Val) = Operator("$sum", arg)

  // id = field
  // id = field in subset
  def apply(id: Field[_], pairs: (Field[_], Operator)*) = {
    val l = (Document.DocumentId -> Eq(id)) +: pairs
    gen( (Query.empty /: l) { (q, t) => q && fieldAsQuery.tupled(t) } )
  }

  // id = document
  def apply(q: Query, pairs: (Field[_], Operator)*) = {
    val doc = Document.DocumentId.build(q)
    gen( (doc /: pairs) { (q,t) => q && fieldAsQuery.tupled(t) } )
  }
}

/** `\$sort`
  */
object Sort extends PipelineOperator("$sort") {
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})
}