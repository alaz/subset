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

import com.mongodb.DBObject

import query._
import update._
import Mutation._
import QueryMutation._

/** Subset wraps facilities for working with subdocuments.
  *
  * Since MongoDB documents may be nested, they sometimes make up a complex hierarchies
  * with arrays of maps or maps of maps.
  *
  * The easiest way to create a `Subset` object is via a helper String pimp:
  * {{{
  * val subset = "votes".subset(Vote).of[List[Vote]]
  * }}}
  *
  * === Queries and update modifiers ===
  * An example:
  * {{{
  * case class Comment(by: String, votes: Int, text: String)
  * case class BlogPost(title: String, comments: List[Comment])
  *
  * object Comment {
  *   val by = "by".fieldOf[String]
  *   val votes = "votes".fieldOf[Int]
  *   val text = "text".fieldOf[String]
  * }
  *
  * object BlogPost {
  *   val title = "title".fieldOf[String]
  *   val comments = "comments".subset(Comment).of[List[Comment]]
  * }
  * }}}
  *
  * `BlogPost.comments` represents an array of `Comment` sub-documents. From now on, you can
  * create queries and update modifiers using the fields defined in `Comment`:
  * {{{
  * BlogPost.comments.where { _.by === 10 }
  * }}}
  * will result in a query `{"comments.by": 10}`.
  *
  * Analogously, update operation
  * {{{
  * BlogPost.comments.modify {_.vote inc 1}`
  * }}}
  * will result in `{\$inc: {"comments.vote": 1}}`
  *
  * === Serialization ===
  * `Subset` provides a couple of helper methods for serializing and deserializing
  * a document from `DBObject`.
  *
  * `apply` takes an object and creates a [[com.osinka.subset.Mutation]],
  * assuming we have a type class [[com.osinka.subset.ValueWriter]] in scope.
  * For example,
  * {{{
  * BlogPost.comments(new Comment("Joe", 2, "text")).get
  * }}}
  * will result in
  * `{"by": "Joe", "votes": 2, "text": "text"}`
  *
  * `unapply` makes use of [[com.osinka.subset.ValueReader]] to extract
  * an object. E.g. assuming we have implicit in scope,
  * {{{
  * dbObject match {
  *   case BlogPost.comments(comments) =>  ...
  * }
  * }}}
  *
  * === Cloning ===
  * You can create a conditional operator for a specific array element, e.g.
  * {{{
  * BlogPost.comments(1).modify {_.by set "john"}
  * }}}
  * creates an update modifier `{\$set: {"comments.1.by": "john"}}`
  *
  * Method `matched` creates a positional query, e.g.
  * {{{
  * BlogPost.comments.matched.modify {_.by set "john"}
  * }}}
  * creates an update modifier `{\$set: {"comments.$.by": "john"}}`
  *
  * === Querying and Updating ===
  * Along with the ordinary queries every field supports, `Subset` lets you define
  * much more advanced
  * [[http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch elemMatch]]
  * queries and
  * [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator positional]]
  * update.
  *
  * Again, turning back to our example, how would you increment "votes" of a Joe's comment?
  *
  * {{{
  * // Let's increment a Joe's comment vote
  * coll.update(BlogPost.comments.where {_.by === "joe"},
  *             BlogPost.comments.matched.modify {_.votes inc 1})
  * }}}
  *
  * It is also possible to find a BlogPost which has a comment made by "Joe" ''and'' having "votes"
  * equal to 2:
  *
  * {{{
  * // Find a blog post commented by Joe and the comment's vote is 2
  * coll.find(BlogPost.comments.elemMatch {comment => comment.by === "joe" && comment.votes === 2}).iterator
  * }}}
  *
  * == `pullWhere` ==
  * `pullWhere` lets specify a [[com.osinka.subset.query.Query]] to select an object
  * that needs to be removed from an array, e.g.
  *
  * {{{
  * collection.update(BlogPost.title === "test",
  *                   BlogPost.comments.pullWhere{comment => comment.by === "user2" && comment.votes === 0})
  * }}}
  *
  * @param self is a fields container. Usually this is an object where a subdocument's fields reside.
  * @tparam Self a type of fields container
  * @see [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
class Subset[T,Self](override val path: List[String], val self: Self) extends Field[T](path) {
  def apply(i: Int): Subset[T,Self] = new Subset[T,Self](path :+ i.toString,  self)

  override def matched: Subset[T,Self] = new Subset[T,Self](path :+ "$", self)

  /** Wrap a query into this subset
    */
  def where(f: Self => Query): Query = where( f(self) )

  /** Create a projection relative to this field (in Aggregation framework)
    */
  def build(f: Self => Query): Query = project( f(self) )

  /** Creates a query as an \$elemMatch relative to this document
    *
    * [[http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch Advanced Queries - elemMatch]]
    */
  def elemMatch(f: Self => Query): Query = elemMatch(f(self))

  /** Creates an update operator positioned relative to this document
    *
    * [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator Updating - The positional operator]]
    */
  def modify(f: Self => Update): Update = modify(f(self))

  /** `\$pull` using a query
    */
  def pullWhere(f: Self => Query)(implicit ev: T <:< Traversable[_]): Update = pullWhere(f(self))

  override def prefixString: String = "Subset"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Subset[_,_] => super.equals(other) }
}

object Subset {
  def apply[T,Self](name: String, self: Self) = new Subset[T,Self](name :: Nil, self)
}
