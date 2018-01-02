package com.github.debasish83.discrete

import java.util

import scala.collection.mutable.ArrayBuffer
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
    while (i < cards.length) {
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
    while (i < k) {
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

  def smallestKSelection(elems: Array[Int], k: Int): Array[Int] = {
    if (k <= 0 || k > elems.length) throw new IllegalArgumentException

    //select the element with rank = k
    val elemK = select(elems, 0, elems.length - 1, k)

    val smallestK = Array.fill[Int](k)(0)
    var i = 0
    while (i < k) {
      if (elems(i) < elemK) smallestK(i) = elems(i)
      i += 1
    }
    smallestK
  }

  // Missing number:

  //Count of 2s: Method to count number of 2s between 0 and n
  // 0 , 1232 1232 has 2 2's

  def numberOf2s(i: Int): Int = {
    var count = 0
    var k = i
    while (k > 0) {
      if (k % 10 == 2) count += 1
      k = k / 10
    }
    count
  }

  def numberOf2sRange(n: Int): Int = {
    var count = 0
    for (i <- 2 until n) {
      count += numberOf2s(i)
    }
    count
  }

  //Optimized solution
  //112 just scan each one as a string and check if there is 2 in it :-)
  //Make a character array of size based on N

  //Iterate through each digit in the number of count 2, do it through maths OR
  //do it through string

  //Baby names; List of baby names and their frequencies
  //There is also a synonym list
  //Names: John(15), Jon(12), Chris(13), Kris(4), Christopher(19)
  //Synonyms: (Jon, John), (John, Johnny), ...

  //First we build a map for Jon -> John, John -> Johnny,... then we traverse the map and
  //generate connected components to find true popular

  import java.util.HashMap
  import java.util.HashSet

  import scala.collection.JavaConverters._

  class GraphException extends Exception

  class Graph {
    // We will maintain an adjacency list for each name
    val adjList = new HashMap[String, Array[String]]()
    val frequencies = new HashMap[String, Int]()

    def addNode(name: String, frequency: Int): Boolean = {
      if (adjList.containsKey(name)) return false
      ???
    }

    def addEdge(node1: String, node2: String): Unit = {
      ???
    }

    def getNeighbors(name: String): Array[String]

    def getNodes: Iterator[String] = {
      adjList.keySet().iterator().asScala
    }

    def getData(node: String): Int = {
      if (frequencies.containsKey(node)) frequencies.get(node)
      else -1
    }
  }

  def constructGraph(names: Map[String, Int]): Graph = {
    val graph = new Graph()
    names.foreach(name => graph.addNode(name._1, name._2))
    graph
  }

  import java.util.Stack

  // We use a stack to do the DFS recursion
  def getComponentFrequency(graph: Graph, node: String, visited: HashSet[String]): Int = {
    var sum = 0
    val stack = new Stack[String]()
    stack.push(node)
    while (stack.isEmpty) {
      val node = stack.pop()
      sum += graph.getData(node)
      val neighbors = graph.getNeighbors(node)
      for (i <- 0 until neighbors.length) {
        if (!visited.contains(neighbors(i))) stack.push(neighbors(i))
      }
    }
    return sum
  }

  //First we create nodes with all the names and then connect the edges
  def getPopular(names: Map[String, Int], synonyms: Map[String, String]): HashMap[String, Int] = {
    val graph = constructGraph(names)
    synonyms.foreach(edge => graph.addEdge(edge._1, edge._2))

    // For connected component analysis, let's start a per component frequency calculation
    val visited = new HashSet[String]()
    val rootNames = new HashMap[String, Int]()

    val iter = graph.getNodes
    while (iter.hasNext) {
      val node = iter.next()
      if (!visited.contains(node)) {
        val frequency = getComponentFrequency(graph, node, visited)
        rootNames.put(node, frequency)
        visited.add(node)
      }
    }
    rootNames
  }

  // Circus tower: There are a collection of people with height and widht. Max people in the tower

  case class Person(height: Int, weight: Int) extends Comparator[Person] {
    override def compare(o1: Person, o2: Person): Int = {
      o1.height - o2.height
    }
  }

  import java.util.ArrayList


  def canAppend(buf: ArrayBuffer[Person], htwt: Person): Boolean = {
    ???
  }

  //13, 14, 10, 12, 15
  //13, 14, 15
  //10, 12
  def longestWeightSeq(persons: Array[Person],
                       buf: ArrayBuffer[Person],
                       index: Int): Array[Person] = {
    // TODO: Add recursion end
    if (index >= persons.length) {
      val pattern = buf.toArray
      buf.clear()
      return pattern
    }

    val htwt = persons(index)

    // buf has set of Person in it, we want to use the same buf generating all the
    // possibilities with increasing weight
    var appended: Array[Person] = null

    // Let the weights be 13, 14, 10, 12, 15
    // 13, Array(14, 10, 15, 12)

    // (13, 14), Array(10, 15, 12)

    //At this point I need to start another recursion with 10
    if (canAppend(buf, htwt)) {
      buf += htwt
      appended = longestWeightSeq(persons, buf, index + 1)
    }

    ???
  }

  // longest increasing in height and weight
  // recursive algorithm is going to O(2^n), it's better to think on dynamic programming
  // line
  def longestIncreasing(persons: Array[Person]): Array[Person] = {
    val heightSorted = persons.sorted

    //First we sort by height and then we find a sequence of increasing weights
    //such that the added weight is lesser than current weight

    val weightBuf = new ArrayBuffer[Person]()

    val weightSeq = longestWeightSeq(heightSorted, weightBuf, 0)
    //Once we have sorted in height, then we have to find a sequence of increasing
    //weights and add the person in a sequence
    ???
  }

  // Iterative version
  // A: 13, 14, 10, 11, 12
  // If we have longest subsequence that terminates with A(0) till A(3), can we find longest subsequence
  // at A(4)
  // Longest subsequence ending at A(0): 13
  // Longest subsequence ending at A(1): 13, 14
  // Longest subsequence ending at A(2): 10
  // Longest subsequence ending at A(3): 10, 11
  // Longest subsequence ending at A(4): 10, 11, 12

  // Sometime for DP one array is sufficient, other cases we may need a 2D array for keeping
  // both indices


  def canAppend(buf: Array[Person], htwt: Person): Boolean = {
    ???
  }

  def bestSeqAtIndex(persons: Array[Person],
                     solutions: Array[Array[Person]],
                     index: Int): Array[Person] = {
    var bestSeq: Array[Person] = Array.empty[Person]
    val htwt = persons(index)
    for(i <- 0 until index) {
      val solution = solutions(i)
      if (canAppend(solution, htwt)) {
        bestSeq = max(solution, bestSeq)
      }
    }
    
    bestSeq.clone() + htwt
    ???
  }

  def max(left: Array[Person], right: Array[Person]): Array[Person] = {
    ???
  }

  def longestIncreasingIterative(persons: Array[Person]): Array[Person] = {
    val sorted = persons.sorted

    // We need to create a sequence for each index
    val solutions = Array.ofDim[ArrayBuffer[Person]](persons.length)
    var longestSeq: Array[Person] = null

    for(i <- 0 until persons.length) {
      val bestAtIndex = bestSeqAtIndex(sorted, solutions, i)
      solutions(i) = bestAtIndex
      longestSeq = max(longestSeq, bestAtIndex)
    }
    longestSeq
  }

  // i = 0, i < str.length/2 (odd, even) n/2
  // (0, 5) => (1, 4) => (2, 3)
  // Do a dynamic programming formulation ? (0, 5) (2, 3) and (1,4)
  // a b a c a
  // 
  def isPalindromeLinear(str: String,
                         memoize: Set[String]): Boolean = {
    val len = str.length/2
    var i = 0
    while(i < len) {
      val start = i
      val end = str.length - i

      if (str(start) == str(end)) {
        i += 1

      }
      val substr = str.substring(start, end)
      if (memoize.contains(substr)) return true
      else {

      }
    }
    ???
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
