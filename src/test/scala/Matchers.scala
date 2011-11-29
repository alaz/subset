package com.osinka.subset

import reflect.{Manifest, ClassManifest}
import org.scalatest.matchers.{Matcher,MatchResult}
import com.mongodb.DBObject

/**
 * FIXME: We cannot do dbo.toString : https://jira.mongodb.org/browse/JAVA-478
 * FIXME: We cannot simply compare values : https://jira.mongodb.org/browse/JAVA-479
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
          // FIXME: must be "dbo.get(key) == value"
          MatchResult(m.erasure.getName == v.getClass.getName && value.toString == v.toString,
                      /*dbo+*/" does not contain key-value "+(key,value)+", but the value is "+value,
                      /*dbo+*/" contains key-value "+(key,value))
        case false =>
          val v = dbo.get(key)
          // FIXME: must be "dbo.get(key) == value"
          MatchResult(m.erasure.getName == v.getClass.getName && value.toString == v.toString,
                      /*dbo+*/" does not contain key-value "+(key,value),
                      /*dbo+*/" contains key-value "+key)
      }
  }

  def containKeyValue[A : Manifest](tuple: (String, A)) = new TupleMatcher(tuple._1, tuple._2)

  class ConformMatcher[A](implicit m: Manifest[A]) extends Matcher[AnyRef] {
    override def apply(obj: AnyRef) = {
      // TODO: https://gist.github.com/748122
      MatchResult(m >:> ClassManifest.singleType(obj),
                  obj+" is not subtype of "+m.erasure,
                  obj+" is a subtype of "+m.erasure)
    }
  }

  def conformTo[A](implicit m: Manifest[A]) = new ConformMatcher[A]()

/*
  case class ObjectHolder[L](val obj: L, val manifest: Manifest[L])

  def typeOf[L](o: L)(implicit m: Manifest[L]) = ObjectHolder(o, m)
  
  class ConformMatcher[L,A](implicit m: Manifest[A]) extends Matcher[ObjectHolder[L]] {
    override def apply(holder: ObjectHolder[L]) = {
      MatchResult(m >:> holder.manifest,
                  holder.obj+" is not subtype of "+m.erasure,
                  holder.obj+" is a subtype of "+m.erasure)
    }
  }

  def conformTo[L,A](implicit m: Manifest[A]) = new ConformMatcher[L,A]()
*/
}
