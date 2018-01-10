package com.github.debasish83.discrete

import com.github.debasish83.discrete.Strings.{unique, uniqueWhitelist}
import org.scalatest.{FunSuite, ShouldMatchers}

class StringsSuite extends FunSuite with ShouldMatchers {
  test("unique string parsing") {
    // step1: Construct the regex [,!]+ regex = "[,! ]+"
    val str1 = "Hello World World"
    val out1 = unique(str1)
    println(out1)

    val str2 = "Hello  World"
    val out2 = unique(str2)
    println(out2)

    val str3 = "Hello,   WORLD! World"
    val out3 = uniqueWhitelist(str3)
    println(out3)
  }
}
