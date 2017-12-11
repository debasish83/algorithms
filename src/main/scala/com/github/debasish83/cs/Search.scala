package com.github.debasish83.cs

import java.util.{Arrays => JArrays}

/**
  * @author debasish83 on 12/3/17.
  */

object Search {
  def binary(elems: Array[Int], a: Int): Boolean = {
    val index = JArrays.binarySearch(elems, 0, elems.size - 1, a)
    if (index > 0) return true
    else return false
  }

  def binaryImpl(elems: Array[Int], a: Int, low: Int, high: Int): Int = {
    if (high <= low) {
      if (a > elems(low)) return low + 1 else return low
    }
    val mid = (low + high) / 2
    if (a == elems(mid)) return mid + 1
    if (a > elems(mid)) binaryImpl(elems, a, mid + 1, high)
    else binaryImpl(elems, a, low, mid - 1)
  }

  def binarySearchRecursive(elems: Array[Int], x: Int, low: Int, high: Int): Int = {
    if (low > high) return -1
    val mid = (low + high) / 2
    if (elems(mid) < x) binarySearchRecursive(elems, x, mid + 1, high)
    else if (elems(mid) > x) binarySearchRecursive(elems, x, low, mid - 1)
    else return mid
  }

  /*
   * find 5 in { 15, 16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14 }
   * Array has been sorted and rotated
   * Let's find mid of the array, if the mid
   * {10, 15, 20, 0, 5}
   * {50, 5, 20, 30, 40}
   * low = 0, high = 4 mid = 2, 20
   * Now since low < mid and target < mid, look in (low, mid - 1)
   * else look in (mid + 1, high)
   * high > mid
   * Since low < mid, [low, mid) should be increasing and 5 is not in it
   * high > mid
   * {2, 2, 2, 3, 4, 2}, find 3
   * low = 0, 2 high = 5, 2 mid = 2 , 2 Now low < mid search [mid + 1, high]
   * If mid < high search[low, mid - 1]
   * Here both are satisfied and we can't prune one side
   */
  def searchSorted(elems: Array[Int], low: Int, high: Int, target: Int): Int = {
    val mid = (low + high) / 2
    if (elems(mid) == target) return mid
    if (high < low) return -1 // The element is not found
    // Left part is ordered, now in the left part we have to see the order of target
    if (elems(low) < elems(mid)) {
      if (target >= elems(low) && target < elems(mid)) // search left
        searchSorted(elems, low, mid - 1, target)
      else searchSorted(elems, mid + 1, high, target) //search right
    }
    // Right part is ordered and find t
    else if (elems(mid) < elems(high)) {
      if (target < elems(high) && target >= elems(mid))
        searchSorted(elems, mid + 1, high, target) // search right
      else searchSorted(elems, low, mid - 1, target) // search left
    }
    else if (elems(low) == elems(mid)) {
      if (elems(high) != elems(mid)) {
        searchSorted(elems, mid + 1, high, target)
      } else {
        // here we can't use the sort ordering and we have to search both sides
        val result = searchSorted(elems, low, mid - 1, target)
        if (result == -1) searchSorted(elems, mid + 1, high, target)
        else result
      }
    }
    return -1
  }

  /*
   Array like data structure Listy which lacks size method. elementAt(i) method returns element at
   index i. Since the size is not known, we can't use binary search. Can we guess the size ? We start
   with a size guess and then
   */
  class Listy(elems: Array[Int]) {
    def elementAt(index: Int): Int = {
      if (index < elems.size) return elems(index)
      else -1
    }

    // we keep a lowp and highp
    // mid = (lowp + highp) / 2
    // elementAt(mid) may be -1 OR higher than target then we update highp to be mid - 1
    // otherwise increment lowp to be mid + 1
    def search(low: Int, high: Int, target: Int): Int = {
      var lowp = low
      var highp = high
      while (lowp <= highp) {
        val mid = (lowp + highp) / 2
        val midElem = elementAt(mid)
        if (midElem > target || midElem == -1) {
          highp = mid - 1
        } else if (midElem < target) {
          lowp = mid + 1
        } else {
          return mid
        }
      }
      return -1
    }
  }

  def searchSortedUnbounded(list: Listy, target: Int): Int = {
    //Use exponential backoff to find upper bound
    var index = 1
    while(list.elementAt(index) != -1 && list.elementAt(index) < target) {
      index *= 2
    }
    list.search(index/2, index, target)
  }

  /*
   * Sparse search: sorted array of strings which has empty string find location of a string
   */

  def findClosest(elems: Array[String], low: Int, high: Int, mid: Int): Int = {
    var lowp = low
    var highp = high
    if (elems(mid).isEmpty) {
      lowp = mid - 1
      highp = mid + 1
      while (true) {
        if (lowp < low && highp > high) return -1 // TODO: Is it && OR || ?
        else if (lowp >= low && !elems(lowp).isEmpty) return lowp
        else if (highp <= high && !elems(highp).isEmpty) return highp
        lowp -= 1
        highp += 1
      }
    }
    return -1
  }

  def search(elems: Array[String], low: Int, high: Int, target: String): Int = {
    if (low > high) return -1
    val mid = findClosest(elems, low, high, (low + high) / 2)
    if (elems(mid) == target) return mid
    else if (elems(mid).compareTo(target) < 0)
      return search(elems, mid + 1, high, target)
    else
      return search(elems, low, mid - 1, target)
  }

  def stringSearch(elems: Array[String], target: String): Int = {
    search(elems, 0, elems.length, target)
  }

  //TODO: Come back to it after dynamic programming
  //20 GB file at input should be partitioned into N part files
  //We load each part file to memory and sort it and write it back
  //After that we load part_i, part_i+1 sort them and write out part_i and then
  //incremental sort part_i+2 with part_i+1. At any point part_k, part_k+1 is in RAM
  //To implement it we will use mergeSort/quickSort routine
  def externalSort(input: String, output: String): Unit = {
    ???
  }

  import scala.io.Source

  //Given an input file with 4 billion non-negative integers, provide an algorithm to
  //generate an integer that's not in the file
  def findDistinct(input: String): Unit = {
    // 4 billion bits java.util.BitSet can't handle, we have to implement our own
    // import java.util.BitSet
    // val bs = new BitSet(Integer.MAX_VALUE)
    val numberOfInts = Integer.MAX_VALUE.toLong + 1
    // 2^32
    // 1 byte has 8 bit and so it can hold numberOfInts/8
    val bitField = Array.fill[Byte]((numberOfInts / 8).toInt)(0x0)

    val iter = Source.fromFile(input).getLines()
    while (iter.hasNext) {
      val num = iter.next().toInt
      //bitField(num/8) picks the byte corresponding to the num, after that we set the nth
      //bit of a byte using OR operator
      bitField(num / 8) = (bitField(num / 8).toInt | (1 << (num % 8))).toByte
      // num%8 is remainder we shift 1 that many time to right 01000000 and then do bitwise OR with
      // existing bits
    }

    //Find all the integer that are not contained
    val bits = (0 until 8)
    var i = 0
    while (i < bitField.length) {
      val byte = bitField(i)
      bits.foreach((j) => {
        if ((byte & (1 << j)) == 0) {
          // bitwise AND to extract 1 << j shift to 01000000 and then & pick out if the byte is available
          println(i * 8 + j)
        }
      })
      i += 1
    }
  }

  //Given a matrix where row and columns are sorted find an element in it
  //Since row and columns are sorted we go over each row / column and see if
  //the last element is > target, if it is then skip the row / column
  def matrixSearch(elems: Array[Array[Int]], target: Int): Int = {
    var i = 0
    while (i < elems.length) {
      val row = elems(i)
      val len = row.length
      if (row(len - 1) < target) i += 1
      else {
        val index = JArrays.binarySearch(row, 0, len - 1, target)
        if (index != -1) return index
        i += 1
      }
    }
    return -1
  }

  /*
   * Peaks and Valleys: peak is an element greater than or equal to adjacent integers
   * and valley is less than or equal to adjacent integers.
   * {5, 8, 6, 2, 3, 4, 6} 8,6 are peaks, {5,2} are valleys
   *
   * Input: {5, 3, 1, 2, 3}
   * Output is alternating sequence of peaks and valleys
   * Output: {5, 1, 3, 2, 3}
   */

  // 1 2 3 3 5
  // Start with 2 1 < 2 < 3 ok
  // 1 2 3 3 5 swap 3 and 5
  // 1 2 3 5 3

  // Another example is an unsorted array
  // 7 9 4 1 0 8
  // Let's sort it first
  // 0 1 4 7 8 9
  // 0 is ok
  // 1 left = 0, right = 4 swap 0, 1, 4, 7, 8, 9
  // 4 left = 1, right = 7 4 is ok
  // 7 left = 4, right = 8 4 < 7 < 8 7 is wrong place swap with 4 0 1 7 4 8 9

  // The algorithm is as follows
  // 1. Sort the array in ascending order
  // 2. iterate from index = 1 and swap 2 elements at a time
  // 3. if small <= medium <= large property is fullfilled then sawp(small, medium)

  def swap(elems: Array[Int], left: Int, i: Int): Unit = {
    val tmp = elems(left)
    elems(left) = elems(i)
    elems(i) = tmp
  }

  def sortValleyPeak(elems: Array[Int]): Unit = {
    if (elems.length == 1) return
    JArrays.sort(elems, 0, elems.length - 1)
    var i = 1
    while (i < elems.length) {
      val left = elems(i - 1)
      val right = elems(i + 1) // TODO: We many need a check here
      if (left <= elems(i) && right > elems(i)) swap(elems, left, i)
      i += 1
    }
  }

  //TODO: Optimal version that runs in O(n)
}


/* Track rank from a stream of numbers
  getRankOfNumber(x: Int)
  track(x: Int)
  5 1 4 4 5 9 7 13 3
  getRankOfNumber(1) = 0
  getRankOfNumber(3) = 1
  getRankOfNumber(4) = 2

  Solution 1: We keep a linkedlist in sorted order from the stream. When a new elemnt comes we find where to insert
  it. After that getRankOfNumber is binary search. Insert is O(n)

  Solution2: To optimize on the insert, we can use a BinarySearchTree, insert is O(logn) getRank is through traversal
  which is also O(logn)

  SortedSet can be used if frequency is not important
  SortedMap can be used if frequnecy also needs to be tracked
 */
class RankTree {

  class Node(val elem: Int) {
    var left: Node = null
    var right: Node = null
    var frequency: Int = 0
    var leftSize: Int = 0
  }

  var root: Node = null

  def insert(iter: Node, x: Int): Unit = {
    // possibilities are that iter.left is null OR iter.right is null
    if (iter.elem == x) iter.frequency += 1
    else if (iter.elem < x) {
      //iter
      if (iter.left != null) insert(iter.left, x)
      else iter.left = new Node(x)
      iter.leftSize += 1
    } else {
      if (iter.right != null) insert(iter.right, x)
      else iter.right = new Node(x)
    }
  }

  def track(x: Int): Unit = {
    if (root == null) root = new Node(x)
    else insert(root, x)
  }

  // Left rank is based on the left size that we have
  // On right side of subtree it has to be 1 + leftSubtree size + rightRank
  // We can extract the size using a O(logn) traversal as well
  def getRank(iter: Node, x: Int): Int = {
    if (iter.elem == x) return iter.leftSize
    else if (x < iter.elem) {
      if (iter.left == null) return -1
      else return getRank(iter.left, x)
    } else {
      val rightRank =
        if (iter.right == null) -1
        else getRank(iter.right, x)
      if (rightRank == -1) return -1
      else return iter.leftSize + 1 + rightRank
    }
  }

  def getRank(x: Int): Int = {
    if (root != null) getRank(root, x)
    else return -1
  }
}

import java.util.{LinkedList => JLinkedList}

class RankList {

  case class Node(elem: Int, frequency: Int)

  val elems = new JLinkedList[Node]()

  def find(low: Int, high: Int, d: Int): Int = {
    var lowp = low
    var highp = high

    // 2 4 7 8 10
    // mid = 4/2 = 2, 7
    // 5 < 7
    // Algorithm1: Do a linear scan to find where to insert O(n)
    // Algorithm2: Do a binary search to find out where to insert
    // 2 4 7 8 10 mid = 2, elems(mid) = 7
    while (lowp < highp) {
      val mid = (low + high) / 2
      if (elems.get(mid).elem > d) {
        if (elems.get(mid - 1).elem < d) return mid
        //(lowp, mid - 1)
        highp = mid - 1
      } else {
        //elems.get(mid).elem < d
        if (elems.get(mid + 1).elem > d) return mid
        //(mid+1, highp)
        lowp = mid + 1
      }
    }
    return -1
  }

  def insert(d: Int): Unit = {
    if (elems.size > 0) {
      val index = find(0, elems.size - 1, d)
      assert(index != -1)
      elems.add(index, Node(d, 0))
    }
  }

  //TODO: Implement getRank function in O(logn)

  //Find magic index: It's an index such that A(i) = i
  //Give a sorted array of distinct integers, write a method to find magic index
  def magicLinear(elems: Array[Int]): Int = {
    var i = 0
    while(i < elems.length) {
      if (elems(i) == i) return i
    }
    return -1
  }

  // 3, 3, 4, 4, 5, 6, 7, 9
  // 7 / 2 = 3, 4 a(3) == 4
  def magicLog(elems: Array[Int], low: Int, high: Int): Int = {
    if (high < low) return -1
    val mid = (low + high) / 2
    if (elems(mid) == mid) return mid
    else if(elems(mid) > mid) return magicLog(elems, low, mid - 1)
    else {
      return magicLog(elems, mid + 1, high)
    }
  }

  def magicLog(elems: Array[Int]): Int = {
    magicLog(elems, 0, elems.length - 1)
  }

  // What happen if the elements are not distinct ?
  // -10, -5, 2, 2, 2, 3, 4, 7, 9, 12, 13
  //   0,  1, 2, 3, 4, 5, 6, 7, 8, 9,  10
  // mid = 5, elems(5) = 3 elems(4) can't be magicIndex because elems(4) <= elems(5)
  // min(elems(5), mid - 1)

  def magicDuplicates(elems: Array[Int], low: Int, high: Int): Int = {
    if (high < low) return -1

    val midIndex = (low + high) / 2
    val midValue = elems(midIndex)
    if (midValue == midIndex) return midIndex //Found magic index

    //Search left
    val leftIndex = Math.min(midIndex - 1, midValue)
    val left = magicDuplicates(elems, low, leftIndex)
    if (left >= 0) return left

    //Search right
    val rightIndex = Math.max(midIndex + 1, midValue)
    val right = magicDuplicates(elems, rightIndex, high)

    return right
  }

  def magicDuplicates(elems: Array[Int]): Int = {
    magicDuplicates(elems, 0, elems.length - 1)
  }

}
