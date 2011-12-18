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

import reflect.{Manifest, ClassManifest}
import org.scalatest.matchers.{Matcher,MatchResult}
import com.mongodb.DBObject

/**
 * FIXME: We cannot do dbo.toString : https://jira.mongodb.org/browse/JAVA-478
 */
trait MongoMatchers {
  class FieldMatcher(key: String) extends Matcher[DBObject] {
    override def apply(dbo: DBObject) =
      MatchResult(dbo.containsField(key),
                  /*dbo+*/" does not contain "+key,
                  /*dbo+*/" contains "+key);
  }

  def containField(key: String) = new FieldMatcher(key)

  class TupleMatcher[A](val key: String, val value: A)(implicit m: Manifest[A]) extends Matcher[DBObject] {
    override def apply(dbo: DBObject) =
      dbo.containsField(key) match {
        case true => 
          val v = dbo.get(key)
          MatchResult(value == v,
                      /*dbo+*/" does contain key "+key+", but the value is "+v,
                      /*dbo+*/" contains key-value "+(key,value))
        case false =>
          MatchResult(false,
                      /*dbo+*/" does not contain key-value "+(key,value),
                      /*dbo+*/" contains key-value "+key)
      }
  }

  def containKeyValue[A : Manifest](tuple: (String, A)) = new TupleMatcher(tuple._1, tuple._2)
}
