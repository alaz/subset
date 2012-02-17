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
package com.osinka.subset.query

trait ConjunctionStrategy {
  def conj(q1: Query, q2: Query): Query
}

object ConjunctionStrategy {
  object Auto extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query = {
      import collection.JavaConversions._

      lazy val s1 = q1.get.keySet
      lazy val s2 = q2.get.keySet

      if (s1.isEmpty) q2
      else if (s2.isEmpty) q1
      else
        (q1, q2) match {
          case (Query.And(ql1), Query.And(ql2)) =>
            Query.And(ql2 ::: ql1)

          case (Query.And(ql1), _) =>
            Query.And(q2 :: ql1)

          case (_, Query.And(ql2)) =>
            Query.And(ql2 ::: q1 :: Nil)

          case _ if (s1 & s2).isEmpty =>
            Query(q1.queryMutation ~ q2.queryMutation)

          case _ =>
            Query.And(q2 :: q1 :: Nil)
        }
    }
  }

  object AndQuery extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query = {
      lazy val s1 = q1.get.keySet
      lazy val s2 = q2.get.keySet

      if (s1.isEmpty) q2
      else if (s2.isEmpty) q1
      else
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
  }

  object Override extends ConjunctionStrategy {
    override def conj(q1: Query, q2: Query): Query = Query(q1.queryMutation ~ q2.queryMutation)
  }
}
