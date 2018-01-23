package com.github.debasish83.practice

import org.scalatest.{FunSuite, ShouldMatchers}

class Practice4Suite extends FunSuite with ShouldMatchers {

  test("topo sort") {
    val words = Array("ccda", "ccbk", "a", "ab")
    val g = new Practice4.Graph(words)
    //a -> b
    //a ??
    println(g)
    val chars = g.topologicalSort()
    println(chars.mkString(","))
  }
}
