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

  def isSymmetric(elems: ArrayList[Int]): Boolean = {
    //Check if elems and it's reverse are same with pointers
    ???
  }

  import java.util.Comparator

  //elems: 1, 1, 2
  //f(1 , (1,2))
  //f(1,  (1,2))
  //f(2,  (1,1))

  //elems: 1 2 4 3
  //1, (2, 4, 3)
  //2, (1, 4, 3)

  //1, 2, 3, 2, 1
  //2, 3,

  //3
  //(0,0) -> 1
  //(1, 1) -> 2

  // (1, 2, 3, 4) ->
  class ArrayComparator extends Comparator[ArrayList[Int]] {
    //1*10000 + 2 * 1000 +
    //
    override def compare(o1: ArrayList[Int], o2: ArrayList[Int]): Boolean = {
      ???
    }
  }

  //HashMap
  //Key -> ArrayList(Value)
  //hash(Key)

  //key1, key2
  //hash(key1) = hash1
  //hash(key2) = hash1
  //hash1 -> (value1, value2)
  //hash1 -> (value1, ..., valuen)
  //O(n) scan on values
  // Java : Tree elems
  // SortedList, BinarySearch

  import scala.collection.mutable.Set

  def getPermutations(elems: ArrayList[Int],
                      pattern: ArrayList[Int],
                      patterns: Set[ArrayList[Int]]): Unit = {
    //Base
    if (elems.size == 0) {
      if (!patterns.contains(pattern)) {
        patterns += pattern.clone().asInstanceOf[ArrayList[Int]]
      }
      return
    }

    // /Recursion
    //index = 0
    //curr = 1

    //(1, 3, 2, 4)
    //1, (3, 2, 4)
    //3, (1, 2, 4)

    // 1 3 2 1 3 4
    for (i <- 0 until elems.size) {
      val curr = elems.get(i)
      pattern.add(curr)
      elems.remove(i)
      //Memoize to stop calling getPermutations
      //pattern has partial pattern
      //pattern: 1,3 (2)
      //pattern: 1,3 (4)
      getPermutations(elems, pattern, patterns)
    }
  }

  //total elems: N
  //N, N-1, N-2
  //O(N^2*N)

  import java.util.ArrayList

  def getPermutations(elems: ArrayList[Int]): Boolean = {
    val patterns = Set[ArrayList[Int]]()
    val pattern = new ArrayList[Int]()
    getPermutations(elems, pattern, patterns)
    patterns.filter(isSymmetric(_))
    ???
  }
}
