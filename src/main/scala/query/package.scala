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

/** == Field Queries ==
  * Typical query starts from a field. The simplest condition is an equality
  * test (see [[com.osinka.subset.query.FieldConditions]]).
  * 
  * It is possible to create several tests on the same field (an example is
  * a range `f > 3 <= 10`) or negate a condition, see
  * [[com.osinka.subset.query.FieldQuery]] for details.
  * 
  * == Composing Queries ==
  * It is possible to compose queries with `&&`, `||` and `nor` methods.
  * See [[com.osinka.subset.query.Query]] for details.
  * 
  * == Subset ==
  * [[com.osinka.subset.Subset.elemMatch]] method allows creating `\$elemMatch`
  * queries, when you need to match all the conditions on a specific subdocument.
  * 
  * {{{
  * val query = BlogPost.Comments.elemMatch { comment =>
  *   comment.by === "joe" && comment.votes === 2
  * }
  * 
  * val blogPosts = collection.find(query)
  * }}}
  * 
  * @see [[com.osinka.subset.query.Query]], [[com.osinka.subset.query.FieldQuery]],
  *      [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
package object query