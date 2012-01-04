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
import DBObjectLens._
import QueryLens._

/** Subset wrap facilities for working with subdocuments.
  *
  * Since MongoDB documents may be nested, they sometimes make up a complex hierarchies
  * with arrays of maps or maps of maps.
  *
  * The easiest way to create a `Subset` object is via a helper String pimp:
  * {{{
  * val subset = "votes".subsetOf[List[Vote]]
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
  *   val comments = "comments".subsetOf[List[Comment]]
  * }
  * }}}
  * 
  * `BlogPost.comments` is a `Subset`. From now on, you may create queries and update operators using these fields
  * and they will have long names in "dot notation", e.g.
  * {{{
  * BlogPost.comments.where { Comment.by === 10 }
  * }}}
  * will result in a query `{"comments.by": 10}`. Analogously, update operation
  * {{{
  * BlogPost.comments.modify{Comment.vote inc 1}`
  * will result in `{\$inc: {"comments.vote": 1}}`
  * 
  * === Serialization ===
  * `Subset` provides a couple of helper methods for serializing an arbitrary number
  * of fields under the subset's name and for deserializing a document from `DBObject`
  * using the subset's name
  * 
  * `apply` takes an object and, assuming we have implicit
  * [[com.osinka.subset.ValueWriter]] in scope, creates a
  * [[com.osinka.subset.DBObjectLens]] storing the object into a fresh `DBObject`
  * with subset's name. For example,
  * `Comments(new Comment("Joe", 2, "text")).get` will result in
  * `{"by": "Joe", "votes": 2, "text": "text"}` (you may find
  * [[com.osinka.subset.ValueWriter]] in the Gist)
  * 
  * Analogously, `unapply` makes use of [[com.osinka.subset.ValueReader]] to extract
  * an object. E.g. assuming we have implicit in scope (see the Gist for example),
  * `dbo match { case Comments(comment) =>  ...  }`
  *
  * === Querying and Updating ===
  * Along with simple queries all fields support, `Subset` lets you define much more
  * advanced
  * [[http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch elemMatch]]
  * queries and
  * [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator positional]]
  * update.
  * 
  * Again, turning back to our example, how would you increment "votes" of a Joe's comment?
  *
  * {{{
  * // Let's increment a Joe's comment vote
  * coll.update(BlogPost.Comments.by === "joe", BlogPost.Comments.updateMatch {_.votes inc 1})
  * }}}
  * 
  * (there is an alternative shown in [[com.osinka.subset.Field]])
  * 
  * It is also possible to find a BlogPost which has a comment made by "Joe" ''and'' having "votes"
  * equal to 2:
  * 
  * {{{
  * // Find a blog post commented by Joe and the comment's vote is 2
  * coll.find(BlogPost.Comments.elemMatch {comment => comment.by === "joe" && comment.votes === 2}).iterator
  * }}}
  *
  * === Tracing ===
  * Since both `Subset` and [[com.osinka.subset.Field]] are [[com.osinka.subset.Path]], you may
  * check what "dot notation" ''Subset'' creates:
  * {{{
  * BlogPost.Comments.votes.at(1).in(BlogPost.Comments).longName must equal("comments.1.votes")
  * }}}
  *
  * @see [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
class Subset[T,Self](override val path: List[String], val self: Self) extends Field[T](path) {
  def apply(i: Int): Subset[T,Self] = new Subset[T,Self](path :+ i.toString,  self)

  override def matched: Subset[T,Self] = new Subset[T,Self](path :+ "$", self)

  def where(f: Self => Query): Query = where( f(self) )

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

  def pullWhere(f: Self => Query)(implicit ev: T <:< Traversable[_]): Update = pullWhere(f(self))

  override def prefixString: String = "Subset"

  override def equals(o: Any): Boolean =
    PartialFunction.cond(o) { case other: Subset[_,_] => super.equals(other) }
}

object Subset {
  def apply[T,Self](name: String, self: Self) = new Subset[T,Self](name :: Nil, self)
}
