package com.github.debasish83.practice

import org.scalatest.{FunSuite, ShouldMatchers}

class Practice1Suite extends FunSuite with ShouldMatchers {
  test("most people alive") {
    val p1 = Practice1.Person(1700, 1790)
    val p2 = Practice1.Person(1790, 1790)
    val p3 = Practice1.Person(1790, 1850)
    val census = Array(p1, p2, p3)
    Practice1.mostPeopleAlive(census)
  }
}
