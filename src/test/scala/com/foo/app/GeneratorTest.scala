package com.foo.app

import org.scalatest.{FreeSpec, Matchers}

class GeneratorTest extends FreeSpec with Matchers {
  import Generator._

  "should return correct error msg in empty input" in {
    val expectedResult = "Invalid data for input string: \"\". Only [1,2,3," +
      "4,5,6,7,8] that can be spited by '-'"

    action("") shouldEqual expectedResult
  }

  "should return correct error msg in bad input" in {
    val expectedResult = "Invalid data for input string: \"a2321\". Only [1," +
      "2,3,4,5,6,7,8] that can be spited by '-'"

    action("a2321") shouldEqual expectedResult
  }

  "should return correct error msg in not allowed input numbers" in {
    val expectedResult = "Invalid input data. Number(s): [1] not allowed. " +
      "Only [1,2,3,4,5,6,7,8] that can be spited by '-'"

    action("1239") shouldEqual expectedResult
  }

  "should return correct msg in empty vocabulary" in {
    vocabulary = Set.empty[String]
    val expectedResult = "No words combinations found"

    action("2345") shouldEqual expectedResult
  }

  "should return correct msg in no combination found" in {
    val expectedResult = "No words combinations found"
    vocabulary = Set("ad")
    action("23-4") shouldEqual expectedResult
  }

  "should return right combination" - {
    "two input numbers separated by \"-\"" in {
      vocabulary = Set("ad", "bf", "a", "d")
      val expectedResult = "a-d, ad, bf"
      action("2-3") shouldEqual expectedResult
    }
    "two input numbers" in {
      vocabulary = Set("ad", "bf", "a", "d")
      val expectedResult = "a-d, ad, bf"
      action("23") shouldEqual expectedResult
    }
    "five input numbers separated by \"-\"" in {
      vocabulary = Set("ad", "ig", "hkn", "aei", "lno", "lo", "ilo", "cf", "beh", "kn", "behkn")
      val expectedResult = "ad-hkn, ad-ilo, aei-kn, aei-lo, beh-kn, beh-lo, behkn, cf-hkn, cf-ilo"
      action("23456") shouldEqual expectedResult
    }
    "five input numbers" in {
      vocabulary = Set("ad", "ig", "hkn", "aei", "lno", "lo", "ilo", "cf", "beh", "kn", "behkn")
      val expectedResult = "ad-hkn, ad-ilo, aei-kn, aei-lo, beh-kn, beh-lo, behkn, cf-hkn, cf-ilo"
      action("23456") shouldEqual expectedResult
    }
  }

}
