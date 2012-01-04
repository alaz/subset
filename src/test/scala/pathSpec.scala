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
class pathSpec extends Spec with MustMatchers with MongoMatchers with Routines {
  describe("Path") {
    it("must have an empty value") {
      Path.empty.path must be('empty)
    }
    it("may be created out of a String") {
      val p = Path("field")
      p.toString must startWith("Path")
      p must equal(Path("field"))
      p must equal(Path("field" :: Nil))
      p.hashCode must equal(Path("field").hashCode)
      p.path must equal(List("field"))
      p.longName must equal("field")
    }
    it("may be created out of a List[String]") {
      val p = Path("doc" :: "field" :: Nil)
      p.toString must startWith("Path")
      p.longName must equal("doc.field")
      p.path must equal("doc" :: "field" :: Nil)
    }
  }
}
