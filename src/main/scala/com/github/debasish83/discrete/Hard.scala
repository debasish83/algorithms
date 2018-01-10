package com.github.debasish83.discrete

import scala.collection.mutable.Set
import scala.util.Random

/**
  * @author debasish83 on 12/4/17.
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

    def getNeighbors(name: String): Array[String] = {
      ???
    }

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
      // heights are not equal, sort based on height
      if (o1.height != o2.height) return o1.height - o2.height
      // if heights are equal, sort based on weight
      o1.weight - o2.weight
    }

    def before(p: Person): Boolean = {
      if (height < p.height && weight < p.weight) return true
      else false
    }
  }

  //13, 14, 10, 12, 15
  //13, 14, 15
  //10, 12

  // persons is sorted array based on height and weight
  // First sort by height and then by weight
  // Now let's see the sort order on the weight
  // 13, 14, 10, 12, 15
  // index = 0 , buf is empty, 13 can be added to buf
  // index = 1 , add(14, buf = {13}) if it can be added, then add 14 and continue
  // index = 2,  add(10, buf = {13, 14}) 10 can't be added so start another call with 10

  import java.util.ArrayList

  // Returns the largest increasing sequence
  /*
  def longestWeightRecurse(persons: Array[Person]): ArrayList[Person] = {
    val sorted = persons.sorted
    val index = 0
    val buf = new ArrayList[Person]()
    longestWeightRecurse(sorted, buf, index)
  }
  */
  def longestWeightRecurse(persons: Array[Person],
                           buf: ArrayList[Person],
                           index: Int): ArrayList[Person] = {
    if (index >= persons.length) {
      return buf
    }
    val htwt = persons(index)
    var seqWithAdd: ArrayList[Person] = null

    // 13 14 12 10 15
    // buf = (13, 14)
    // index = 2, 12 12 can't be added to buf
    // buf, index + 1
    // index = 2 12 if 12 can be added to buf then create a new buf

    // 12 it can't add and so it need to start another one
    if (canAdd(buf, htwt)) {
      val bufAdded = buf.clone().asInstanceOf[ArrayList[Person]]
      bufAdded.add(htwt)
      seqWithAdd = longestWeightRecurse(persons, bufAdded, index + 1)
    }
    val seqWithoutAdd = longestWeightRecurse(persons, buf, index + 1)

    // Find the max within seqWithAdd and seqWithoutAdd, return the max
    if (seqWithAdd == null || seqWithoutAdd.size > seqWithAdd.size) {
      return seqWithoutAdd
    } else {
      return seqWithAdd
    }
  }

  def canAdd(buf: ArrayList[Person], htwt: Person): Boolean = {
    //this case should not come
    if (buf == null) return false
    //base case: no elements in the buffer
    if (buf.isEmpty) return true

    // If it can add then it will recurse
    // it will recurse with new one
    // recurse with 13, recurse with 14, recurse with 10 so on and so forth
    // At each point it goes 2 options: 2^n horrible algorithm
    val last = buf.get(buf.size - 1)
    last.before(htwt)
  }

  // Iterative version
  // A: 13, 14, 10, 11, 12
  // Longest increasing ending at A(0) = 13
  // Longest increasing at A(1) = 13, 14
  // We can keep the last element and index on heap to get logn in place of linear scan
  // Longest increasing at A(2) = max (Longest increasing at A(0) + elems(2), Longest increasing at A(1) + elems(2))

  // If we have longest subsequence that terminates with A(0) till A(3), can we find longest subsequence
  // at A(4)
  // Longest subsequence ending at A(0): 13
  // Longest subsequence ending at A(1): 13, 14
  // Longest subsequence ending at A(2): 10
  // Longest subsequence ending at A(3): 10, 11
  // Longest subsequence ending at A(4): 10, 11, 12

  // For Dynamic programming there are two possibilities
  // A subproblem is defined at each index of the sequence
  // A subproblem is defined at index_i and index_j of the sequence
  def max(left: ArrayList[Person], right: ArrayList[Person]): ArrayList[Person] = {
    if (left == null) return right
    else if (right == null) return left
    if (left.size > right.size) return left
    else return right
  }

  def longestAtIndex(persons: Array[Person],
                     solutions: Array[ArrayList[Person]],
                     index: Int): ArrayList[Person] = {
    val htwt = persons(index)
    var bestIncreasing: ArrayList[Person] = null
    // TODO: find the closest head from the htwt using a heap that maintain element
    // and index. For now we do a linear scan
    for (i <- 0 until index) {
      val solution = solutions(i)
      if (canAdd(solution, htwt)) {
        bestIncreasing = max(bestIncreasing, solution)
      }
    }
    val appendedBest = bestIncreasing.clone().asInstanceOf[ArrayList[Person]]
    appendedBest.add(htwt)
    appendedBest
  }

  /*
  def longestIncreasingIterative(persons: Array[Person]): ArrayList[Person] = {
    val sorted = persons.sorted

    val solutions = Array.ofDim[ArrayList[Person]](persons.length)
    var bestSequence: ArrayList[Person] = null

    for (i <- 0 until persons.length) {
      val bestAtIndex = longestAtIndex(sorted, solutions, i)
      solutions(i) = bestAtIndex
      bestSequence = max(bestAtIndex, bestSequence)
    }
    return bestSequence
  }
  */
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
