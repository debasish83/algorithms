package com.github.debasish83.discrete

import org.scalatest.{FunSuite, ShouldMatchers}

class StackSuite extends FunSuite with ShouldMatchers {
  test("is compile") {
    // "{abc }abc{ab}"

    val str = "{abc}abc[ab]"

    assert(Stack.isCompiles(str) == "compiles")

    val str2 = "{abcd]"

    assert(Stack.isCompiles(str2) == "!compiles")

    val str3 = "}abc]"

    assert(Stack.isCompiles(str3) == "!compiles")
  }
}
