package com.github.debasish83.discrete

import org.scalatest.{FunSuite, ShouldMatchers}

class ArraysSuite extends FunSuite with ShouldMatchers {
  test("pair sum") {
    val elems = Array(4, 3, 4, 7, 1, 8, 0)
    val out = Arrays.pairSum(elems, 8)
    println(out.mkString(","))
  }
}
