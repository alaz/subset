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