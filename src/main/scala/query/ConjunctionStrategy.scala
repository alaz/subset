package com.osinka.subset.query

trait ConjunctionStrategy {
  def conj(q1: Query, q2: Query): Query
}

object ConjunctionStrategy {
  object Auto extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query = {
      import collection.JavaConversions._

      (q1, q2) match {
        case (Query.And(ql1), Query.And(ql2)) =>
          Query.And(ql2 ::: ql1)

        case (Query.And(ql1), _) =>
          Query.And(q2 :: ql1)

        case (_, Query.And(ql2)) =>
          Query.And(ql2 ::: q1 :: Nil)

        case _ if (q1.get.keySet & q2.get.keySet).isEmpty =>
          Query(q1.queryMutation ~ q2.queryMutation)

        case _ =>
          Query.And(q2 :: q1 :: Nil)
      }
    }
  }

  object AndQuery extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query =
      (q1, q2) match {
        case (Query.And(ql1), Query.And(ql2)) =>
          Query.And(ql2 ::: ql1)

        case (Query.And(ql1), _) =>
          Query.And(q2 :: ql1)

        case (_, Query.And(ql2)) =>
          Query.And(ql2 ::: q1 :: Nil)

        case _ =>
          Query.And(q2 :: q1 :: Nil)
      }
  }

  object Override extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query = Query(q1.queryMutation ~ q2.queryMutation)
  }
}
