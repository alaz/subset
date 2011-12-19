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

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.mongodb.BasicDBObjectBuilder.start

@RunWith(classOf[JUnitRunner])
class lensSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("DBObjectLens") {
    // FIXME: cannot compare arrays https://jira.mongodb.org/browse/JAVA-482
    def dbo = start("i", 10).push("inner").add("s", "string")/*.add("a", Array(1,2))*/.get
    def lens = DBObjectLens(_ => dbo)

    it("has `equals`") {
      lens must equal(lens)
      lens.get must equal(dbo)
    }
    it("has `hashCode`") {
      lens.hashCode must equal(lens.hashCode)
      lens.get.hashCode must equal(dbo.hashCode)
    }
    it("has `toString`") {
      lens.toString must startWith("DBObjectLens")
    }
    it("initialized from empty DBObject") {
      DBObjectLens(identity).get must be('empty)
    }
    it("has ValueWriter") {
      val w = implicitly[ValueWriter[DBObjectLens]]
    }
    it("allows conjunction") {
      val conj = lens ~ DBObjectLens(identity)
      conj.toString must startWith("DBObjectLens")
      conj must equal(lens)
    }
  }
}
