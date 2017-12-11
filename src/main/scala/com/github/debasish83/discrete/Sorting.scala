package com.github.debasish83.discrete

/**
  * @author debasish83 on 11/15/17.
  */

import java.util.Comparator

import com.github.debasish83.discrete.Arrays.Pixel

object Sorting {

  // For bubble do the loop go from i = 0, j = i ?
  // i = 0, j = 0, j=1, j=2,...
  // i = 1, j = 0, j=2, ...

  // 5, 4, 3, 2

  // Pass1:
  // i = 0, j = 1, 2, 3
  // 4, 3, 2, 5
  // Pass2:
  // i = 1, j = 0, 1, 2, 3
  // 4, 2, 3, 5 (but now 4 is lost in sort order, we can't do this)

  def swap(elems: Array[Int], i: Int, j: Int): Unit = {
    val tmp = elems(i)
    elems(i) = elems(j)
    elems(j) = tmp
  }

  def bubbleSort(elems: Array[Int]): Unit = {
    var swapped = false
    var n = elems.length
    while (swapped) {
      swapped = false
      var i = 0
      while (i < n) {
        if (elems(i) > elems(i + 1)) {
          swap(elems, i, i + 1)
          swapped = true
        }
        i += 1
      }
      n = n - 1
    }
  }

  def findMin(elems: Array[Int], start: Int, end: Int): Int = {
    var i = start
    var min = elems(start)
    var index = start
    while (i < end) {
      if (min < elems(i)) {
        min = elems(i)
        index = i
      }
      i += 1
    }
    index
  }

  // In each iteration we find the min from unsorted part and add it to the sorted part
  def selectionSort(elems: Array[Int]): Unit = {
    val len = elems.length
    var i = 0
    while (i < len) {
      val index = findMin(elems, i + 1, len)
      swap(elems, i, index)
      i += 1
    }
  }

  // Pass1: find smallest element and move it to front
  // O(nlogn)
  // For each element, find smallest elements from the remaining
  // 5, 4, 3, 2
  // i = 0 (5) findMin(4, 3, 2)
  // min = 3
  // val leftMin = findMin(4) = 2
  // val rightMin = findMin(2) = 4
  // if 3 < 2 return 2
  // if 3 < rightMin return rightMin
  // if 3 < findMin(4)
  // Example 12, 11, 13, 5, 6

  // Insertion sort with binary search
  // 12, 11, 13, 5, 6
  // a[1] = 11
  // Where should 11 be inserted in elems[0, 1] ?
  // loc = 0
  // 0, 1 is sorted
  // 0, 2 is sorted
  // 0, 3 is sorted
  // After that we move rest of the elements to make space
  // while( j >= loc ) {
  //    a[j + 1] = a[j]

  //TODO: Go through the algorithm once again with an example
  def insertionSort(elems: Array[Int]): Unit = {
    val len = elems.length
    var i = 1
    while (i < len - 1) {
      var j = i - 1
      val selected = elems(i)
      // Uses the property that the array from 0 to j is sorted, if the array is not sorted
      // binarySearch is not applicable
      val loc = Search.binaryImpl(elems, selected, 0, j)
      while (j >= loc) {
        elems(j + 1) = elems(j)
        j -= 1
      }
      elems(j + 1) = selected
    }
  }

  def mergeSort(elems: Array[Int], helper: Array[Int], low: Int, high: Int): Unit = {
    if (low < high) {
      val mid = (low + high) / 2
      mergeSort(elems, helper, low, mid) // sort left
      mergeSort(elems, helper, mid + 1, high) // sort right
      merge(elems, helper, low, mid, high) // merge them
    }
  }

  def merge(elems: Array[Int], helper: Array[Int], low: Int, mid: Int, high: Int): Unit = {
    // Use helper to do the merge and copy helper to elems
    // (low, mid) is sorted in elems
    // (mid + 1, high) is sorted in elems
    // Copy low, high to helper and then make elems sorted
    for (i <- low to high) {
      helper(i) = elems(i)
    }
    var leftp = low
    var rightp = mid + 1
    var current = low

    // leftp will increment till mid and rightp will increment till high
    while (leftp <= mid && rightp <= high) {
      // element on left is smaller than element on right, increment leftp
      if (helper(leftp) <= helper(rightp)) {
        elems(current) = helper(leftp)
        leftp += 1
      } else {
        // element on right is smaller than element on left, increment rightp
        elems(current) = helper(rightp)
        rightp += 1
      }
      current += 1
    }

    // copy the rest of the left side of the array to target array
    val remaining = mid - leftp
    var i = 0
    while (i < remaining) {
      elems(current + i) = helper(leftp + 1)
      i += 1
    }
    // Right side is already sorted
  }

  def mergeSort(elems: Array[Int]): Unit = {
    val helper = Array.fill[Int](elems.length)(-1)
    mergeSort(elems, helper, 0, elems.length - 1)
  }

  // For quickSort, we pick a random element and then partition the array
  // all numbers less than partitioning element come before all elements that
  // are greater than it
  def partition(elems: Array[Int], left: Int, right: Int): Int = {
    val pivot = elems((left + right) / 2) // Pivot point can be randomized

    var leftp = left
    var rightp = right

    // 7, 4, 5, 6, 3
    // pivot is 5
    // element which are less than 5 should be on left
    // element which are more than 5 should be on the right
    // left should have element < pivot
    // right should have element > pivot

    // Maintain a left and right pointer, increment left and right till we get violation
    while (leftp <= rightp) {
      while (elems(leftp) < pivot) leftp += 1
      while (elems(rightp) > pivot) rightp -= 1

      // We got a potential swap chance
      if (left <= rightp) {
        swap(elems, leftp, rightp)
        leftp += 1
        rightp -= 1
      }
    }
    leftp
  }

  def quickSort(elems: Array[Int], left: Int, right: Int): Unit = {
    val index = partition(elems, left, right)
    // index is within left and right
    if (left < index - 1) quickSort(elems, left, index - 1)
    if (index < right) quickSort(elems, index, right)
  }

  // Radix sort logic: O(kn)
  // 47, 24, 35, 16, 43
  // max digits = 2
  // digit = 0
  // [0, 9] 7, 4, 5, 6, 3 => 43, 24, 35, 16, 47
  // digit = 1
  // [0, 9]
  // Radix sort iterate through each digit of the number, grouping numbers by each digit
  // It needs to go from highest precision
  // 47, 24, 35, 16, 43
  // Pass0
  // 4, 2, 3, 1, 4
  // 4 -> 47, 43
  // 3 -> 35, 33
  // 2 -> 24, 21
  // 1 -> 16, 15

  // Pass1
  // Sort on second digit

  // A and B are both sorted arrays, A has enough size at the end to hold B
  // A = 2, 6, 8, 10
  // B = 3, 4, 7
  // pointerA = 6
  // pointerB = 3
  // since 3 < 6, we have to make space for 3
  // Better is to start from back
  // pointerA = 10, pointerB = 7 10 > 7, we copy 10 at the end, decrement pointerA
  // pointerA = 8, pointerB = 7 8 > 7, we copy 8 at end - 1, decrement pointerA
  // pointerA = 6, pointerB = 7 7 > 6, we copy 7 at end - 2, decrement pointerB
  def sortedMerge(A: Array[Int], B: Array[Int]): Unit = {
    var ptrA = A.length - 1
    var ptrB = B.length - 1
    var current = A.length + B.length - 1

    while(ptrB >= 0) {
      if (ptrA >= 0 && A(ptrA) > B(ptrB)) {
        A(current) = A(ptrA)
        ptrA -= 1
      } else {
        A(current) = B(ptrB)
        ptrB -= 1
      }
      current -= 1
    }
    //Rest of the A we don't have to copy since it is already sorted in order
  }

  val a = List(10, 5, 8, 1, 7)
  class IntOrder extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int = {
      if (x < y) return -1
      else if (x > y) return 1
      else return 0
    }
  }
  a.sorted(new IntOrder)

  // TODO: override Pixel difference and use that to clean complex if-else
  class PixelComparator extends Comparator[Pixel] {
    override def compare(x: Pixel, y: Pixel): Int = {
      if (x.b < y.b && x.g < y.g && x.r < y.r) return -1
      else if (x.b > y.b && x.g > y.g && x.r > y.r) return 1
      else return 0
    }
  }

  // Sort array of strings so that all anagrams are next to each other
  // Anagram definition: Anagram are words that have the same characters but in
  // different order
  // chaair and riahc are not anagram of each other but distinct logic will find them
  // as anagram
  // chair: Map[c -> 1, h -> 1, ... ]
  // Array("chair", "table", "cup", "plate", "puc", "riach")

  // Java style sorting with comparator
  import java.util.HashMap

  class AnagramComparator extends Comparator[String] {
    override def compare(x: String, y: String): Int = {
      val charMap = new HashMap[Char, Int]()
      x.foreach((c: Char) => {
        if (charMap.containsKey(c)) {
          charMap.put(c, charMap.get(c) + 1)
        } else {
          charMap.put(c, 0)
        }
      })

      y.foreach((c: Char) => {
        if (charMap.containsKey(c)) {
          charMap.put(c, charMap.get(c) - 1)
        } else {
          return 1
        }
      })

      val keys = charMap.keySet().iterator()
      while (keys.hasNext) {
        val key = keys.next()
        if (charMap.get(key) != 0) return -1
      }
      return 0
    }
  }

  //Scala style sorting with scala.math.Ordering and mutable.Map

  /*
  Important collections
  scala.collection.immutable:
  Seq, List, Set, BitSet, SortedSet, SortedMap
  Scala Set is HashSet, SortedSet is where set entries are sorted using Ordering provided
  Scala Map is HashMap, SortedMap is where set entries are sorted using Ordering provided
  scala.collection.mutable:
  SortedSet is available, SortedMap is not available here
  java.util.TreeMap and java.util.TreeSet are concrete impl of SortedMap and SortedSet
  How to provide Comparator to TreeMap
  */

  import java.util.TreeMap

  val cmp = new Comparator[Char] {
    override def compare(x: Char, y: Char): Int = {
      if (x < y) return -1
      else if (x > y) return 1
      else return 0
    }
  }

  class AnagramOrdering extends Ordering[String] {
    val charMap = new TreeMap[Char, Int](cmp)

    override def compare(x: String, y: String): Int = {
      x.foreach((c: Char) => {
        if (charMap.containsKey(c)) {
          charMap.put(c, charMap.get(c) + 1)
        } else {
          charMap.put(c, 0)
        }
      })

      y.foreach((c: Char) => {
        if (charMap.containsKey(c)) {
          charMap.put(c, charMap.get(c) - 1)
        } else {
          return 1
        }
      })

      val keys = charMap.keySet().iterator()
      while (keys.hasNext) {
        val key = keys.next()
        if (charMap.get(key) != 0) return -1
      }
      return 0
    }
  }
}
