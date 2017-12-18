package com.github.debasish83.discrete

import com.github.debasish83.discrete.Recursive._
import org.scalatest.{FunSuite, ShouldMatchers}

class RecursiveSuite extends FunSuite with ShouldMatchers {
  test("denoms count") {
    val amount = 63
    println(makeChange(amount))
  }

  test("denoms pattern") {
    val amount = 63
    val patterns = makeChangePatterns(amount)
    println(patterns.length)
  }

  /*
  test("longest unique substring") {

    val arr1 = Array('a', 'b', 'a', 'b', 'c', 'd', 'a')
    val lcs1 = findLCS(arr1)

    //start with a
    //a is distinct
    //a, b is distinct
    //a, b, a is duplicated

    //a
    //a b c d a is not distinct

    //b, a fails
    //a, b is the sequence
    //a, b, c, d, a fails
    //a, b, c, d is the sequence
    assert(lcs1 === Array('b', 'c', 'd', 'a'))

    val arr2 = Array('a', 'a', 'b', 'c')
    val lcs2 = findLCS(arr2)

    assert(lcs2 === Array('a', 'b', 'c'))
  }
  */
}
