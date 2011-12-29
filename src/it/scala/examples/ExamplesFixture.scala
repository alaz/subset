package com.osinka.subset.examples

import org.scalatest.{BeforeAndAfterAll,Suite}

import com.mongodb._

trait ExamplesFixture extends BeforeAndAfterAll {
  self: Suite =>

  val DefaultHostname = "localhost"
  val DefaultDB = "test"
  val DefaultCollection = "test"

  val mongo = new Mongo(DefaultHostname)
  val db = mongo getDB  DefaultDB
  val collection = db getCollection DefaultCollection

  override def beforeAll {
    collection.drop
  }

  override def afterAll {
    collection.drop
    mongo.close
  }
}