package com.github.debasish83.discrete

import scala.util.Random

/**
  * @author v606014 on 12/4/17.
  */

object Hard {

  // Addition without any arithmetic operators
  // We can use bitwise operator
  // n + m 28 + 37 7 + 8 = 15 5 and carry = 1
  // For 7 + 8 0111 1000 If I do bitwise OR 1111
  // 28 + 39 1000 + 1001 10001
  def add(n: Int, m: Int): Int = {
    if (m == 0) return n
    val sum = m ^ n
    val carry = (m & n) << 1
    return add(sum, carry)
  }

  // Shuffle: Write a method to shuffle a deck for card where a random number generator
  // is provided
  def shuffleCards(cards: Array[Int]): Unit = {
    var i = 0
    while(i < cards.length) {
      val index = Random.nextInt(i)
      val tmp = cards(index)
      cards(index) = cards(i)
      cards(i) = tmp
      i += 1
    }
  }

  import java.util.Arrays

  //Smallest K: Find smallest K in an array
  //O(nlogn)
  def smallestK(elems: Array[Int], k: Int): Array[Int] = {
    if (k <= 0 || k > elems.length)
      throw new IllegalArgumentException()
    // Provide an comparator example if interviewer looks for it
    Arrays.sort(elems)
    val kelems = Array.fill[Int](k)(0)
    var i = 0
    while(i < k){
      kelems(i) = elems(i)
      i += 1
    }
    return kelems
  }

  import java.util.PriorityQueue
  import java.util.Comparator

  //Since we want smallest K, we can use a heap and remove the K elements
  //Complexity: O(klogn)

  class MaxHeapComparator extends Comparator[Int] {
    override def compare(x: Int, y: Int): Int = {
      return -(x - y)
    }
  }

  def smallestKHeap(elems: Array[Int], k: Int): Array[Int] = {
    if (k <= 0 || k > elems.length)
      throw new IllegalArgumentException()
    //MaxHeap vs MinHeap will be based on Comparator
    val heap = new PriorityQueue[Int](k, new MaxHeapComparator)
    var i = 0
    while (i < elems.length) {
      if (heap.size < k) {
        //more space to add elements
        heap.add(elems(i))
      } else if (elems(i) < heap.peek()) {
        //elems(i) is smaller than existing max, we can remove the max
        heap.poll()
        heap.add(elems(i))
      }
      i += 1
    }

    val smallestK = Array.fill[Int](k)(0)
    i = 0
    while (i < k) {
      smallestK(i) = heap.poll()
      i += 1
    }
    return smallestK
  }

  /* Selection rank is well-known algorithm to find ith smallest (or largest) element in an
     array in linear time

    4 3 5 2 7 6
    random here is say 5

    k = 4

    4, 3, 2, 5, 7, 6

    All elements on left are less than 5


    Algorithm1: Using a random pivot like quicksort

    1. Pick a random element in the array and use it as a "pivot", Partition elements around pivot keeping track of
    elements on the left side
    2. If there are exactly i elements on the left, return biggest element on the left
    3. If left side is bigger than i, recurse on left part
    4. If left side is smaller than i, recurse on the right but look for rank i - leftSize

    Algorithm2:
    Use radix sort using a direct mapping and give the k-th element from the mapping
  */

  def select(elem: Array[Int], low: Int, high: Int, k: Int): Int = {
    ???
  }

  def smallestKSelection(elems: Array[Int], k: Int) : Array[Int] = {
    if (k <= 0 || k > elems.length) throw new IllegalArgumentException

    //select the element with rank = k
    val elemK = select(elems, 0, elems.length - 1, k)

    val smallestK = Array.fill[Int](k)(0)
    var i = 0
    while(i < k) {
      if (elems(i) < elemK) smallestK(i) = elems(i)
      i += 1
    }
    smallestK
  }
}

class ContinuousMedian {

  // Algorithm1: Insertion is fast, median extraction is O(logn) which is slow
  // as new numbers come in 2, 3, 1, 4, 5, 3 we keep a hashSet where numbers are entered
  // Figure out the median is a linear/binary scan, we start with linear scan

  // Algorithm2: median calculation is constant time due to looking at two heaps
  // adding element takes O(lgn)
  // Existing median = 2,
  def add(elem: Int) = {
    ???
  }

  def get(): Int = {
    ???
  }
}
