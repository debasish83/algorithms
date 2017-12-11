package com.github.debasish83.cs

object OrderAnalysis {
  def power(a: Int, b: Int): Double = {
    var i = 0
    var pow = 0
    while(i < b) {
      pow = pow*a
    }
    pow
  }

  // Calculate sqrt of a number N = 100
  // Sqrt of a number must be smaller than the number N and therefore we use binary search
  // x*x = n
  // sqrt of N = 100 Let's start with 1 + 100 / 2 = 50 50*50 > 100 go 25
  def sqrt_helper(n: Int, left: Int, right: Int): Int = {
    if (right < left) return -1 // no square root
    val guess = (left + right) / 2
    val guesssqr = guess * guess
    if (guesssqr == n) return guess
    else if (guesssqr < n) sqrt_helper(n, guess + 1, right) //higher
    else sqrt_helper(n, left, guess - 1) //lower
  }

  def sqrt(n : Int): Int = sqrt_helper(n, 1, n)

  //Iterative version in binary search complexity
  def sqrtIterative(n: Int): Int = {
    var left: Int = 0
    var right: Int = n
    while(right < left) {
      val guess = (left + right)/2
      val guesssqr = guess*guess

      if (guesssqr == n) return guess
      else if (guesssqr < n) left = guess + 1
      else right = guess - 1
    }
    return -1
  }

  // Generate all strings of length k where the characters are in sorted order
  // First option is from 26 characters, next one is from 25, third one is from 24 and so on
  def generateSorted(k: Int): Unit = {
    val numChars = 26
    val offset = 'a'.toInt
    var i = 0
    while(i < k) {
      var j = 0
      while(j < numChars - i) {
        println((offset+j).toChar)
        j += 1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Hello World")
  }
}
