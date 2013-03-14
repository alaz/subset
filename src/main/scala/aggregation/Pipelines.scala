package com.osinka.subset
package aggregation

import com.mongodb.DBObject

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

  implicit val aggregateFieldWriter = ValueWriter[Field[_]](f => "$"+f.longName)
}

object Project extends PipelineOperator("$project") {
  // default
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  // include / exclude
  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})

  // TODO: populate {stats: {pv: "$f1", p2: "$f2"}}
  //def apply(fields: (Field[_]))
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

// TODO: edge case: is it possible to have dotted field in $unwind?
object Unwind extends PipelineOperator("$unwind") {
  def apply[A <: Traversable[_]](f: Field[A]): DBObject = gen(f)
}

object Group extends PipelineOperator("$group") {
  case class Op(val v: Any)
  object Op {
    def apply[T : ValueWriter](f: String, v: T): Op = new Op(Mutation.writer(f, v) : DBObject)
  }

  def Eq[A : ValueWriter](v: A) = Op( ValueWriter.pack(v).get )
  def AddToSet[A](f: Field[A]) = Op("$addToSet", f)
  def First[A](f: Field[A]) = Op("$first", f)
  def Last[A](f: Field[A]) = Op("$last", f)
  def Max[A](f: Field[A]) = Op("$max", f)
  def Min[A](f: Field[A]) = Op("$min", f)
  def Avg[A](f: Field[A]) = Op("$avg", f)
  def Push[A](f: Field[A]) = Op("$push", f)
  def Sum(i: Int) = Op("$sum", i)
  def Sum[A](f: Field[A]) = Op("$sum", f)

  // TODO: id = dotted field path
  // TODO: see http://docs.mongodb.org/manual/tutorial/aggregation-examples/

  // id = constant
  def apply(id: Field[_], pairs: (Field[_], Op)*) = {
    def writer(p: (Field[_], Op)) = Mutation.writer(p._1.longName, p._2.v)(ValueWriter.anyWriter)
    val l = (Document.DocumentId -> Eq(id)) +: pairs
    gen( (Mutation.empty /: l) { (m, t) => m ~ writer(t) } )
  }

  // TODO: id = document
}

object Sort extends PipelineOperator("$sort") {
  def all(fields: Field[_]*): DBObject =
    apply(fields map {_ -> 1} :_*)

  def apply(fields: (Field[_], Int)*): DBObject =
    gen((Query.empty /: fields) { (q, t) => q && (t._1.int === t._2)})
}