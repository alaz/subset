package com.osinka.subset

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class matcherSpec extends Spec with MustMatchers with MongoMatchers {
  describe("be subtypeOf matcher") {
    it("must conform List[Int] to Seq[Int]") {
      List(1) must conformTo[Seq[Int]]
    }
    it("must not conform String to List") {
      "Aaaa" must not (conformTo[List[String]])
    }
    it("must not conform List[String] to Seq[Int]") {
      pending
      List(1) must not (conformTo[Seq[String]])
    }
    it("must conform List[List[String]] to List[Seq[String]]") {
      List(List("a")) must conformTo[List[Seq[String]]]
    }
    it("must not conform List[Seq[String]] to List[List[String]]") {
      pending
      List(List("a").toSeq) must not (conformTo[List[List[String]]])
    }
  }
}
