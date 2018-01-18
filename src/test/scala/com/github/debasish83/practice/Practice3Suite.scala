package com.github.debasish83.practice

import org.scalatest.{FunSuite, ShouldMatchers}

class Practice3Suite extends FunSuite with ShouldMatchers {

  test("ip addresses") {
    val str = "12345"
    val out = Practice3.generateIps(str, dots = 3)
    out.foreach(println(_))
  }
}
