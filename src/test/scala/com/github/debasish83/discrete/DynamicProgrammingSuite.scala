package com.github.debasish83.discrete

import org.scalatest.{FunSuite, ShouldMatchers}

class DynamicProgrammingSuite extends FunSuite with ShouldMatchers {
  test("substring without duplicates") {
    val str1 = "aabc"
    DynamicProgramming.findLongestDistinct(str1)
  }
}
