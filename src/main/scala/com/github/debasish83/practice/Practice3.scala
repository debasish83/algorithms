package com.github.debasish83.practice

import java.util.ArrayList

import scala.collection.mutable.ArrayBuffer

object Practice3 {
  def generateIps(str: String, dots: Int): Array[String] = {
    val pattern = new ArrayList[String]()
    val patterns = new ArrayList[ArrayList[String]]()
    generateIps(str, 0, dots, pattern, patterns)
    val sb = new StringBuilder()
    val buf = new ArrayBuffer[String]()

    for (i <- 0 until patterns.size) {
      val pattern = patterns.get(i)
      for (j <- 0 until pattern.size - 1) {
        sb.append(pattern.get(j))
        sb.append(".")
      }
      sb.append(pattern.get(pattern.size - 1))
      buf += sb.toString()
      sb.clear()
    }
    buf.toArray
  }

  def generateIps(str: String,
                  index: Int,
                  dots: Int,
                  pattern: ArrayList[String],
                  patterns: ArrayList[ArrayList[String]]): Unit = {
    //str.length = 5
    //12345
    //1 2345
    //12 345
    //dots = 3

    //Base case
    if (dots == 0) {
      //TODO: Next 2 lines were missed
      val substr = str.substring(index, str.length)
      val cloned = pattern.clone().asInstanceOf[ArrayList[String]]
      cloned.add(substr)
      patterns.add(cloned)
      return
    }

    // Recursion
    if (index + 1 < str.length) {
      for (i <- index + 1 until str.length) {
        //index, i construct pattern
        //use dot to stop the recursion
        val substr = str.substring(index, i)
        val diff = str.length - index
        println(s"start: $index end: $i substr: $substr diff: $diff")
        //TODO: diff logic was missed
        if (diff > dots) {
          pattern.add(substr)
          generateIps(str, i, dots - 1, pattern, patterns)
          pattern.remove(pattern.size - 1)
        }
      }
    }
  }
}
