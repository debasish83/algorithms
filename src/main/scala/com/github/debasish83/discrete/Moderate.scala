package com.github.debasish83.discrete

import java.util.Comparator

import com.github.debasish83.discrete.Recursive.Point

import scala.collection.mutable.ArrayBuffer

case class Person(birthYear: Int, deathYear: Int)

//Sort options
//Use scala.util.Sorting for quicksort
//Array[Person].sorted which internally calls java.util.Arrays.sort
//java.util.Arrays.sort Uses legacy merge sort OR TimSort
//scala.util.Sorting implements quicksort

object Moderate {

  // 1800 1870
  // 1870 1870
  // 1870 1950
  class BirthOrder extends Ordering[Person] {
    def compare(x: Person, y: Person): Int = {
      x.birthYear - y.birthYear
    }
  }

  class DeathOrder extends Ordering[Person] {
    def compare(x: Person, y: Person): Int = {
      x.deathYear - y.deathYear
    }
  }

  import java.util.ArrayList
  import scala.collection.mutable.Map
  import java.util.Collections
  import java.util.Arrays

  //Keep an list of keys and respective count in as hashMap, run a binary search
  //in the list of keys to find insertion point and update the keys after that
  //binary search is fine but key update can be a O(n) operation !

  //1800 1870
  //1850 1900
  //1880 1950

  //step1: iterate over birth Year
  //Find 1800 position using binary search and increment count till we reach 1870

  //1850, 1900
  //Find 1850 position using binary search and then increment count till we hit 1900
  class IntCmp extends Comparator[Int] {
    def compare(o1: Int, o2: Int) = o1 - o2
  }

  // If it is Person we can extends Comparator
  def mostPeopleAliveInterval(census: Array[Person]): Int = {
    val years = new ArrayList[Int]()
    val aliveYear = Map.empty[Int, Int]

    val cmp = new IntCmp
    val ord = new BirthOrder

    val birthSorted = census.sorted(ord)
    var i = 0
    while (i < birthSorted.length) {
      //find the index of exact match or index of insert
      //Arrays.binarySearch gives an insertion point if it does not exist
      val index = Collections.binarySearch(years, birthSorted(i).birthYear, cmp)
      var iter = if (index < 0) {
        // if key does not exist -(<i>insertion point</i>) - 1)
        -index - 1
      } else {
        index
      }

      while (iter < years.size) {
        if (years.get(iter) <= birthSorted(i).deathYear) {
          aliveYear(years.get(iter)) += 1
        }
        iter += 1
      }

      // insert birthYear at index so that years stay sorted
      if (index < 0) {
        years.add(index, birthSorted(i).birthYear)
        aliveYear.put(birthSorted(i).birthYear, 1)
      }

      // insert deathYear at end of list so that years stay sorted
      if (!aliveYear.contains(birthSorted(i).deathYear)) {
        years.add(birthSorted(i).deathYear)
        aliveYear.put(birthSorted(i).birthYear, 1)
      }
      i += 1
    }
    //aliveYear has year, peopleAlive, we have to sort on peopleAlive and pick up year
    var maxAlive = 0
    var maxYear = 0
    i = 0
    while (i < years.size) {
      val currAlive = aliveYear(i)
      if (currAlive > maxAlive) {
        maxAlive = currAlive
        maxYear = i
      }
      i += 1
    }
    return maxAlive
  }

  def mostPeopleAlive(census: Array[Person]): Int = {
    // sort on birthYear
    // sort on deathYear

    // increment if a birthYear has changed, decrement if we hit a death
    // update the maxAliveYear through some logic

    // Sorting.quickSort(census)
    val minYear = census.map(_.birthYear).min

    val iord = Ordering[Int]
    val birthSorted = census.map(_.birthYear).sorted(iord)
    val deathSorted = census.map(_.deathYear).sorted(iord)

    var birthIndex = 0
    var deathIndex = 0

    var maxAlive = 0
    var currentAlive = 0
    var maxAliveYear = minYear

    while (birthIndex < birthSorted.length) {
      if (birthSorted(birthIndex) <= deathSorted(deathIndex)) {
        currentAlive += 1
        if (currentAlive > maxAlive) {
          maxAlive = currentAlive
          maxAliveYear = birthSorted(birthIndex)
        }
        birthIndex += 1
      } else if (birthSorted(birthIndex) > deathSorted(deathIndex)) {
        currentAlive -= 1
        deathIndex += 1
      }
    }
    return maxAliveYear
  }

  // Number swapper: Function to swap a number in place

  //computes the number of trailing zeros in n factorial
  //Algorithm1: Implement n! with BigNumber and then 4! = 4*3*2*1
  //BigNumber(1) * 2 = BigNumber
  //BigNumber * 3 = BigNumber so on and so forth
  //a: BigNumber * n can be written as:
  // var b = new BigNumber(0)
  // for(i <- 0 until n) b = b + a

  //Algorithm2: 19! = 19 * ... * 16 * 15 * ... * 10 * ... * 5 * ... * 2
  //Go through each number and find number of times 5 shows up 5 -> 1, 10 -> 1, 15 -> 1 20 -> 1
  //Since multiples of 2 and 5 contribute to 0, we need to count the pairs of 2 and 5
  //Multiples of 2 are larger than 5, 2, 4, 8, 12 etc are all multiples of 5 and so we count
  //multiples of 5
  def countMultiples5(num: Int) : Int = {
    var count = 0
    var iter = num
    while(iter % 5 == 0) {
      iter = iter / 5
      count += 1
    }
    count
  }

  def countZeros(num: Int): Int = {
    var count = 0
    for (i <- 2 until num) {
      count += countMultiples5(i)
    }
    count
  }

  // smallest difference: 2 arrays of integers compute the pair of values with the smallest
  // non-negative difference
  // Input: {1, 3, 5, 11, 2} {23, 127, 235, 19, 18}

  // algorithm1: brute force m x n and select arr1(i) - arr2(j) and then update the min

  // algorithm2: Sort input1 and input2 and then traverse them
  // 1 2 3 5 11
  // 18 19 23 127 235
  // One array start with end, another one start with beginning
  // A: {1, 2, 11, 15}
  // B: {4, 12, 19, 23, 127, 235}
  // sortA: {1, 2, 11, 15}
  // sortB: {4, 12, 19, 23, 127, 235}
  // iterA, iterB
  // 1 - 4 = 3 min = 3 itrA = 1, itrB = 4 which one I should move ?
  // itrB++ > itrB, itrA++ > itrA,
  // moving itrB will make difference larger so move the pointer that's smaller
  def findSmallestDifference(arr1: Array[Int], arr2: Array[Int]): Int = {
    Arrays.sort(arr1)
    Arrays.sort(arr2)

    var itr1 = 0
    var itr2 = 0
    var mindiff = Int.MaxValue

    while (itr1 < arr1.length && itr2 < arr2.length) {
      val diff = Math.abs(arr1(itr1) - arr2(itr2))
      if (diff < mindiff) {
        mindiff = diff
      }
      if (arr1(itr1) > arr2(itr2)) {
        itr2 += 1
      } else {
        itr1 += 1
      }
    }
    mindiff
  }

  //method to find max of two numbers without if-else/comparison

  def flip(bit: Int): Int = {
    return 1 ^ bit
  }

  def sign(a: Int): Int = {
    //right shift 31 bit so that the sign bit / 32 bit is last
    //1 & 1 = 1 , 0 & 1 = 0
    //flip bit using exor: 1 ^ 1 = 0, 1 ^ 0 = 1

    // a>>31 & 0x1 If a is negative then 1 else 0
    // We want if a is negative 0 else 1, flip using exor
    return flip((a >> 31) & 0x1)
  }

  def getMaxOverflow(a: Int, b: Int): Int = {
    val k = sign(a - b)
    val q = flip(k)
    a * k + b * q
  }

  def getMax(a: Int, b: Int): Int = {
    //diff = a - b
    val diff = a - b

    val sa = sign(a) //if a >= 0 return 1 else 0
    val sb = sign(b) //if b >= 0 return 1 else 0

    // When does a and b overflow ?
    // 1. a is positive b is negative
    // 2. a is negative b is positive

    val sdiff = sign(diff) //sign of difference

    // If a and b have different signs k = sign(a)
    val use_sign_of_a = sa ^ sb

    val use_sign_of_diff = flip(sa ^ sb)

    val k = use_sign_of_a * sa + use_sign_of_diff * diff
    val q = flip(k)

    return a * k + b * q
  }

  // Given a integer 1824 One thousand eight hundred twenty four
  // a(i) = "one", "two", "three", "four} etc
  // 19, 323, 984
  // nineteen million three hunderd twenty three thousand nine hundred eighty four
  // smalls = Array("zero", ..., "nineteen")
  // tens = Array("twenty", "thirty", ...)

  // We break it up into chunks of 1000

  val smalls = Array("zero", "nineteen")
  val bigs = Array("billion", "million", "thousand")

  import java.util.LinkedList

  def convertChunk(num: Int) : String = {
    ???
  }

  def convert(num: Int): String = {
    if (num == 0) return smalls(0)
    else if (num < 0) {
      return "negative " + convert(-num)
    }

    //val parts = new ArrayList[String]()
    //Better to use a linkedlist so that we can pop things out, this will be useful for
    //the carry, sum use-case as well
    val parts = new LinkedList[String]()

    var chunkCount = 0
    var itr = num
    while(itr > 0) {
      if (itr % 1000 != 0) {
        val chunk = convertChunk(itr % 1000) + " " + bigs(chunkCount)
        parts.add(chunk)
      }
      itr /= 1000
      chunkCount += 1
    }

    val sb = new StringBuilder
    while(parts.size > 1) {
      sb.append(parts.pop())
      sb.append(" ")
    }
    sb.append(parts.pop())
    sb.toString()
  }

  // multiply, subtract and divide for integers use add operator
  // a - b = a + (-b)

  def negate(a: Int): Int = {
    var neg = 0
    val newSign = if (a < 0) 1 else -1
    var itr = a
    //TODO: Make this loop faster
    while(itr != 0) {
      neg += newSign
      itr += newSign
    }
    itr
  }

  def minus(a: Int, b: Int): Int = {
    return a + negate(b)
  }

  //multiplication we sum them up using the sign of a and b

  //division
  //x = a / b
  //bx = a
  //How many times we need to add b so that it gets equal to a
  import scala.math.abs

  def divide(a: Int, b: Int): Int = {
    if (b == 0) throw new ArithmeticException("error")

    val absa = abs(a)
    val absb = abs(b)

    var product = 0
    var x = 0
    while (product + absb <= absa) {
      product += absb
      x += 1
    }

    if (a < 0 && b < 0 || a > 0 && b > 0) return x
    else negate(x)
  }

  // bisect squares: 2 square on 2-D place, find a line that will cut these 2 squares in half
  // A line is defined by slope and y-intercept
  // slope = y1 - y2 / x1 - x2
  // y = x*slope + intercept
  // y1 = x1 * (y1 - y2)/(x1 - x2) + intercept
  // intercept = y1 - x1*(y1 - y2)/x1 - x2 = y1(x1 - x2) - x1(y1 - y2) / (x1 - x2)
  // intercept = (x1y2 - x2y1)/(x1 - x2)

  // The line that cuts two squares in half must connect the two mid points
  // Let's calculate the mid of square1 and mid of square2 and then fit the line
  // Line is y = slope * x + intercept

  class Square(left: Int, right: Int, top: Int, bottom: Int) {
    def middle(): Point = {
      Point((left + right) / 2, (top + bottom) / 2)
    }

    def extend(mid1: Point, mid2: Point, size: Double): Point = {
      ???
    }
  }

  //Given a two dimensional graph with points with it, find a line which passes through the
  //most number of points
  //(x1, y1) (x2, y2), ....
  //Foreach combination of (x_i, y_i) (x_j, y_j) find the slope and intercept and compare it with
  //all the rest
  //We are keeping 2 lines are equal if slope and intercept are same but there is an epsilon
  //associated with each slope we keep if two floating points are epsilon of each other
  //flooredSlope, flooredSlope - epsilon, flooredSlope + epsilon
  //HashMap[Int, ArrayList[Int]]

  val eps = 0.0001

  class Line(p: Point, q: Point) {
    var slope = 0
    var intercept = 0
    var infiniteSlope = false

    if (abs(p.x - q.x) > eps) { // x are different
      slope = (p.y - q.y) / (p.x - q.x)
      intercept = p.y - slope * p.x
    } else {
      infiniteSlope = true
      intercept = p.x
    }
  }

  //In Map maintain Map[Int, ArrayList[Line]]

  // Master Mind:  computer has 4 slots R, Y, G, B
  // Computer has RGGB
  // If the guess is YRGB
  // Correct color in correct slot it's an hit
  // If you guess a color but that's in wrong slot then it's pseudo hit
  // R->0
  // G->1,2
  // B->3

  // Y -> 0
  // R -> 1
  // G -> 2
  // B -> 3

  // B is a hit and R, G are pseudohit

  // Another example
  // computer RGBY
  // R -> 0
  // G -> 1
  // B -> 2
  // Y -> 3

  // Guess: GGRR
  // G -> 0, 1
  // R -> 2, 3

  // That's how we keep track

  // subsort: Given Array of integers, find a method to find index m and n so that if you sort them
  // entire array will be sorted

  // 1, 2, 4, 7, 10, 11, 8, 12, 5, 6, 16, 18, 19
  // Longest increasing subsequence from the beginning
  // index = 0 1
  // index = 1 1,2
  // index = 2 max(ArrayList(1,4), ArrayList(1, 2, 4)) = ArrayList(1,2,4)
  // index = 3 max((1,7) (1, 2, 7) (1,2,4,7)) = (1,2,4,7)
  // index = 4 At each step we are doing a max to find the largest sequence
  // it works most likely with positive/negative numbers both but sum won't work on negative numbers

  // lcs from beginning 1 2 4 7 10 11
  // lcs from end 5 6 16 18 19
  // Elements are already in sorted order
  // left -> 8, right -> 12
  // move leftptr such that leftptr < 8
  // move rightptr such that rightptr > 12
  // After that sort (leftprt, rightptr)

  def findLeftEnd(elems: Array[Int]): Int = {
    var i = 1
    while (i < elems.length) {
      // increasing order is ok, decreasing order needs to be flagged
      if (elems(i) < elems(i - 1)) return i
      i += 1
    }
    return elems.length - 1
  }

  def findRightStart(elems: Array[Int]): Int = {
    var i = elems.length - 2
    while (i > 0) {
      if (elems(i) > elems(i + 1)) return i + 1
      i += 1
    }
    return 0
  }

  def moveLeft(elems: Array[Int], minIndex: Int, endLeft: Int): Int = {
    var iter = endLeft - 1
    while (iter >= 0) {
      if (elems(iter) <= elems(minIndex)) return iter + 1
      iter -= 1
    }
    return 0
  }

  def moveRight(elems: Array[Int], maxIndex: Int, startRight: Int): Int = {
    var iter = startRight
    while (iter < elems.length) {
      if (elems(iter) >= elems(maxIndex)) return iter - 1
      iter += 1
    }
    elems.length - 1
  }

  def findUnsorted(elems: Array[Int]): Unit = {
    // find left subsequence
    val endLeft = findLeftEnd(elems)
    // already sorted
    if (endLeft >= elems.length - 1) return

    val startRight = findRightStart(elems)

    //within endLeft and startRight find min and max
    var maxIndex = endLeft
    var minIndex = startRight
    for(i <- endLeft + 1 until startRight) {
      if (elems(i) < elems(minIndex)) minIndex = i
      if (elems(i) > elems(maxIndex)) maxIndex = i
    }

    //move left until less than elems(minIndex)
    val leftIndex = moveLeft(elems, minIndex, endLeft)

    //move right until greater than elems(maxIndex)
    val rightIndex = moveRight(elems, maxIndex, startRight)

    println(s"$leftIndex $rightIndex")
  }

  //Contiguous sequence: given an array of integer (positive & negative) find contiguous with
  //largest sum
  //2, -8, 3, -2, 4, -10
  //index 0 2
  //index 1 -6, -8
  //index 2 -6 + 3, -8 + 3, 2 + 3
  //Since it is a continguous sequence things like 2 + 3 is not allowed but it may be asked that
  //don't find the continguous sequnece but any sequence with max sum (with negative numbers)
  def getMaxSum(elems: Array[Int]): Int = {
    var maxsum = 0
    var sum = 0
    for (i <- 0 until elems.length) {
      sum += elems(i)
      if (maxsum < sum) {
        maxsum = sum
      } else if (sum < 0) {
        sum = 0
      }
    }
    maxsum
  }

  //pattern matching: you are given two strings pattern and value
  //the pattern string consists of letters a and b describing a pattern within string
  //pattern = aabab value = cat cat go cat go
  //Does this pattern aabab exist in value ?

  //catcatgocatgo
  //c
  //ca
  //cat

  //pond sizes, we have a integer matrix representing plot of land, value of zero indicates
  //water. A pond is a region that connected horizontally vectorically and diagonally with water
  //pond sizes is same as the minesweeper game code, where we are checking all combination around
  //the cell (i, j) through the scanning logic

  //20: phone number: 8733
  //8: tuv
  //7: pqrs
  //3: def
  //3: def

  //tree: dictionary match, used: dictionary match
  //put the dictionary in a trie and do the prefix match, if we can't find a prefix, then stop
  //the recursion, the idea is similar to anagram computation but here trie is used to break
  //recursion, in this example let's also fix the Trie code

  def getT9Letters(digit: Int): Array[Char] = {
    digit match {
      case 0 => Array('a', 'b', 'c')
      case _ => throw new ArrayIndexOutOfBoundsException("$digit out of [0-9]")
    }
  }

  def getValidWords(num: String, index: Int,
                    prefix: String, iter: Trie#Node,
                    results: ArrayList[String]): Unit = {
    //Steps
    //1. At the base case check if the TrieNode is terminating (children is null), if it is then
    //   it's a valid dictionary match, add the prefix that was build to the result
    //2. If not continue recursion if a child exist in the trie with the new letter

    //TODO: Add base case
    if (index == num.length) {
      if (iter.children == null) {
        results.add(prefix)
      }
      return
    }

    val digit = num.charAt(index)
    // For this digit 1 2 3 ... 9 there are corresponding letters
    val letters = getT9Letters(digit)
    //this is same as Java9 treatment
    for (letter <- letters) {
      //We should not implement getChild as a HashMap/HashSet. The chars on each node are
      //bounded. A binary search will be efficient to retrieve it
      val child = iter.getChild(letter)
      if (child != null) {
        getValidWords(num, index + 1, prefix + letter, child, results)
      }
    }
  }

  def getValidT9Words(num: String, trie: Trie): ArrayList[String] = {
    val results = new ArrayList[String]()
    getValidWords(num, 0, "", trie.head, results)
    return results
  }

  //Most optimal: go through all words from the dictionary and create a mapping with T9
  //representation
  //tree => 8733
  //8733 => {tree, used, ...}
  //Take a dictionary and generate all possible combinations of dictionary words and assign
  //them to a T9 sequence

  //Pairs with sum: All pairs of integers that sum up to specific value
  //Algorithm1: Generate all pairs and then see if they sum up to W
  //Algorithm2: Do a sort and then apply the following logic
  //After sort: -2, -1, 0, 3, 5, 6, 7, 9, 13, 14
  // start end
  // start + end = W we found a match
  // if start + end != W, then we have to check if s < sum
  // if s < sum, then we have to increase s, start++
  // if s > sum we have to decrease s and so end--

  case class Pair(i: Int, j: Int)

  def getPairSums(elems: Array[Int], w: Int): Array[Pair] = {
    // If these are different data structure like Array[Person] use sorted with Ordering
    Arrays.sort(elems)
    val buf = new ArrayBuffer[Pair]()

    var start = 0
    var end = elems.length - 1
    while (start < end) {
      val sum = elems(start) + elems(end)
      if (sum == w) {
        buf += Pair(start, end)
      } else {
        if (sum < w) {
          //We have to increase the sum, increment start
          start += 1
        } else {
          end -= 1
        }
      }
    }
    buf.toArray
  }

  //Least recently used
  //Keep a hashMap of all the items
  //keep the keys in a sorted way based on timestamp

  //LRU Cache has a few requirements
  //1. Insert key-value pairs
  //2. Retrive value by key: O(c)
  //3. Find least recently used: least recently used item and usage ordering of all items
  //4. Updating most recently used: When a key is retrieved, update the order to be most
  //   recently used
  //5. Eviction: Cache must have a max capacity and as soon as capacity is reached we have to
  //   evict most recently used

  //What's the data structure we should choose for it ?
  //The hashMap goes from key to value where value in this case is a LinkedListNode

  //Insert key-value pair: Create a linked list node with key, value and insert it to the head
  //When retrieve value by key, move the node to the front of list
  //Eviction: remote from the tail
}
