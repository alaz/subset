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
package examples

import org.scalatest.{FeatureSpec,GivenWhenThen}
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.bson.types.ObjectId
import com.mongodb.{DBObject,BasicDBObjectBuilder,WriteResult}
import BasicDBObjectBuilder.start

/** "Blog Post - Comment" example to show subdocuments and updating its fields
  *
  * Run it using `it:test` command in `sbt`.
  *
  * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator MongoDB documentation]]
  */
@RunWith(classOf[JUnitRunner])
class blogCommentSpec extends FeatureSpec with GivenWhenThen with MustMatchers with ExamplesFixture {
  import SmartValues._

  info("The data model represents a blog post, that has a title and a number of")
  info("comments. Every comment has an author, text and number of votes.")

  info("This data model means every MongoDB document (blog post) contains an array")
  info("of sub-documents (comments)")

  case class Comment(by: String, votes: Int, text: String)
  case class BlogPost(title: String, comments: List[Comment])

  object Comment {
    val by = "by".fieldOf[String]
    val votes = "votes".fieldOf[Int]
    val text = "text".fieldOf[String]

    // these two are required so that reading/writing of List[Comment] becomes possible
    implicit val reader = ValueReader[Comment]({
      case by(by) ~ votes(votes) ~ text(text) => new Comment(by, votes, text)
    })
    implicit val writer = {
      def f(comment: Comment): DBObject =
        (by -> comment.by) ~ (votes -> comment.votes) ~ (text -> comment.text)
      ValueWriter(f _)
    }
  }

  object BlogPost {
    val title = "title".fieldOf[String]
    val comments = "comments".subset(Comment).of[List[Comment]]

    def read(dbo: DBObject) =
      PartialFunction.condOpt(dbo) {
        case title(t) ~ comments(clist) => new BlogPost(t, clist)
      }

    def toDBO(blogPost: BlogPost): DBObject =
      (title -> blogPost.title) ~ (comments -> blogPost.comments)
  }

  feature("It is possible to save a BlogPost") {
    scenario("directly") {
      given("there is a blog post")
      val blogPost = BlogPost("title", Comment("joe", 0, "joe's comment") :: Nil)

      when("it is saved into the collection")
      val dbo = BlogPost.toDBO(blogPost)
      successful { collection insert dbo }

      then("since the collection has one record, it's possible to get it")
      val saved = Option( collection.findOne ) flatMap { BlogPost.read _ }
      saved must be ('defined)

      and("this DBObject contains all BlogPost fields")
      saved.get must have( 'title ("title"), 'comments (blogPost.comments) )

      info(" (we shall keep this record)")
    }

    scenario("using Document helper") {
      given("there is a blog post")
      val blogPost = BlogPost("title", Comment("joe", 0, "joe's comment") :: Nil)

      when("there is an implicit BlogPost => DBObject")
      implicit def blogPostToDBO(bp: BlogPost): DBObject = BlogPost toDBO bp

      then("it's possible to call an `insert` operation via Document.newDoc")
      val insertOp = (dbo: DBObject) => collection insert dbo
      val oid = Document.newDoc(blogPost) { insertOp }

      then("the new document's OID is correct")
      oid must be('right)
      
      and("it helps to find the record")
      info(" (there is a caveat: Since `DBCollection.findOne` accepts AnyRef, an ")
      info("  implicit conversion from Query to DBObject will not be triggered")
      info("  Thus we have to tell the compiler what type we need to supply to `findOne`)")
      val record = Option( collection.findOne(Document.DocumentId === oid.right.get : DBObject) ) flatMap { BlogPost.read _ }
      record must be('defined)

      and("this DBObject contains all BlogPost fields")
      record.get must have( 'title ("title"), 'comments (blogPost.comments) )

      info(" (we shall remove this record, so that the collection contains only one)")
      collection.remove(Document.DocumentId === oid.right.get)
    }
  }
  
  feature("It is possible to push more comments") {
    scenario("via $push") {
      given("there is another comment we need to store")
      val comment = Comment("mary", 0, "mary's comment")

      when("it's been added to the blog post comments")
      successful { collection.update(Query.empty, BlogPost.comments push comment) }

      then("the record contains both comments")
      val record = Option( collection.findOne ) flatMap { BlogPost.read _ }
      record must be ('defined)
      record.get.title must equal("title")
      record.get.comments must (contain(comment) and contain(Comment("joe", 0, "joe's comment")))
    }
  }
  feature("It is possible to push to empty field") {
    scenario("via $pushAll") {
      given("there is a blog post without comments")
      val blogPost = BlogPost("empty", Nil)

      when("there is an implicit BlogPost => DBObject")
      implicit def blogPostToDBO(bp: BlogPost): DBObject = BlogPost toDBO bp

      when("it's been stored into the collection")
      val oid = Document.newDoc(blogPost) { (dbo: DBObject) => collection insert dbo }
      oid must be('right)
      
      then("it's possible to push a number of comments at once")
      val comments = secondRecordComments
      successful { collection.update(Document.DocumentId === oid.right.get, BlogPost.comments pushAll comments) }

      and("the record contains all the stored comments")
      secondRecordIntact( Option( collection.findOne(Document.DocumentId === oid.right.get : DBObject) ) flatMap { BlogPost.read _ } )
    }
  }

  feature("It is possible to increment a specific comment's vote") {
    scenario("using $positional in a field") {
      given("the blog posts are stored and has comments")
      collection.count must equal(2)

      when("the specific comment vote count has been incremented")
      successful { collection.update(BlogPost.comments.where{_.by === "joe"}, BlogPost.comments.matched.modify {_.votes inc 1}) }

      then("the record contains the new updated comment")
      val record = Option( collection.findOne(BlogPost.title === "title" : DBObject) )
      record must be ('defined)
      val recordComments = DBObjectLens.read[List[Comment]]("comments", record.get)
      recordComments must be('defined)
      recordComments.get must contain(Comment("joe", 1, "joe's comment"))

      and("the other record is intact")
      secondRecordIntact( Option( collection.findOne(BlogPost.title === "empty" : DBObject) ) flatMap { BlogPost.read _ } )
    }
  }

  feature("It is possible to pull a specific comment") {
    scenario("via $pull by query") {
      given("the blog posts are stored")
      collection.count must equal(2)

      then("we may pull a specific comment")
      successful {
        collection.update(BlogPost.title === "empty",
          BlogPost.comments.pullWhere {comment =>
            comment.by === "user2" && comment.votes === 0})
      }

      and("the object contains only one comment")
      val record = Option( collection.findOne(BlogPost.title === "empty" : DBObject) ) flatMap { BlogPost.read _ }
      record must be('defined)
      record.get.comments.size must equal(1)
      record.get.comments.head must equal(Comment("user1",0,"u1 comment"))
    }

    scenario("via $pull by object") {
      given("the blog posts are stored")
      collection.count must equal(2)

      then("we may pull a specific comment")
      successful {
        collection.update(BlogPost.title === "empty", BlogPost.comments.pull(Comment("user1", 0, "u1 comment")))
      }

      and("the object contains no comments")
      val record = Option( collection.findOne(BlogPost.title === "empty" : DBObject) ) flatMap { BlogPost.read _ }
      record must be('defined)
      record.get.comments must be('empty)
    }
  }

  feature("It is possible to find a Blog post with specific comment") {
    scenario("using $elemMatch") {
      given("the blog posts are stored")
      collection.count must equal(2)

      then("it's possible to find a blog post by a specific comment's fields")
      val query = BlogPost.comments.elemMatch {comment => comment.by === "joe" && comment.votes === 1}
      val record = Option( collection.findOne(query  : DBObject) ) flatMap { BlogPost.read _ }
      record must be('defined)

      and("it has correct comment")
      record.get.comments must contain(Comment("joe",1,"joe's comment"))
    }
  }

  def successful(f: => WriteResult) {
    val wr = f
    wr.getError must equal(null)
  }

  def secondRecordComments = Comment("user1", 0, "u1 comment") :: Comment("user2", 0, "u2 comment") :: Nil

  def secondRecordIntact(record: Option[BlogPost]) {
    record must be ('defined)
    record.get.title must equal("empty")
    record.get.comments must equal(secondRecordComments)
  }
}