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
