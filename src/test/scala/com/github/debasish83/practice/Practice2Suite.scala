package com.github.debasish83.practice

import com.github.debasish83.practice.Practice2.AutoComplete
import org.scalatest.{FunSuite, ShouldMatchers}

class Practice2Suite extends FunSuite with ShouldMatchers {
  test("autocomplete") {
    val dictionary = Array("apple", "bed", "bedroom", "broom", "break", "cat", "corner", "dog", "dogs", "directive")

    val ac = new AutoComplete(dictionary)

    val out1 = ac.find("be")
    for (i <- 0 until out1.size) println(out1.get(i))

    val out2 = ac.find("br")
    for (i <- 0 until out2.size) println(out2.get(i))

    val out3 = ac.find("do")
    for (i <- 0 until out3.size) println(out3.get(i))

    val out4 = ac.find("b")
    for (i <- 0 until out4.size) println(out4.get(i))
  }
}
