package com.github.debasish83.discrete

import java.util.Comparator

import scala.collection.mutable

/**
  * @author debasish83 on 12/3/17.
  */
object Recursive {
  def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    return fib(n - 1) + fib(n - 2)
  }

  def fib(n: Int, memo: Array[Int]): Int = {
    if (n == 0 || n == 1) return n

    if (memo(n) == 0) {
      memo(n) = fib(n - 1, memo) + fib(n - 2, memo)
    }
    return memo(n)
  }

  def fibMemo(n: Int): Int = {
    val memo = Array.fill[Int](n + 1)(0)
    fib(n, memo)
  }

  // A child is running staircase with 1 step, 2 step or 3 steps at a time.
  // Implement a method to count the total number of steps
  // At a given step, the child has 3 possibilities unless n-1/n-2/n-3 equals 0
  def numSteps(n: Int): Int = {
    if (n < 0) // He already reached at the end
      return 0
    else if (n == 0)
      return 1
    else {
      return numSteps(n - 1) + numSteps(n - 2) + numSteps(n - 3)
    }
  }

  import scala.collection.mutable.Map

  def numStepsMemo(n: Int, memo: Map[Int, Int]): Int = {
    if (n < 0) return 0
    else if (n == 0) return 1
    else if (memo.contains(n)) return memo(n)
    else {
      memo.put(n, numStepsMemo(n - 1, memo) + numStepsMemo(n - 2, memo) + numStepsMemo(n - 3, memo))
      return memo(n)
    }
  }

  def numStepsMemo(n: Int): Int = {
    val memo = Map.empty[Int, Int]
    numStepsMemo(n, memo)
  }

  // Robot in a grid: Robot is sitting on the upper left corner of a grid with r rows and c columns
  // It can move right or down, certain cells it can't step on it
  // Let's say the robot is at row r and column c
  // From (r, c) it can move to r-1 or (r, c)
  case class Point(x: Int, y: Int)

  import java.util.ArrayList

  // Tip: Lot of people use variable name x and y when dealing with 2D arrays
  // matrix[x][y] normally it goes as matrix[y][x]

  def getPath(matrix: Array[Array[Boolean]], row: Int, col: Int, path: ArrayList[Point]): Boolean = {
    if (row < 0 || col < 0 || !matrix(row)(col)) // matrix obstructed
      return false
    val isOrigin = (row == 0) && (col == 0)
    // path exists, add to it
    if (isOrigin || getPath(matrix, row - 1, col, path) || getPath(matrix, row, col - 1, path)) {
      path.add(Point(row, col))
      return true
    }
    return false
  }

  // It's an exponential algorithm, memoization should help improve it
  def getPath(matrix: Array[Array[Boolean]]): ArrayList[Point] = {
    if (matrix == null || matrix.length == 0) return null
    val path = new ArrayList[Point]()
    //top if maze(max.length - 1)
    //matrix.length - 1 is the first row
    if (getPath(matrix, matrix.length - 1, matrix(0).length - 1, path)) return path
    return null
  }

  //TODO: Add a method to return all subsets of a set

  //Recursive multiply: multiply two positive integer without *
  //8*7: we will sum 8 7 times This is O(7) Can we do faster ? But not needed

  /*Tower of Hanoi: 3 towers and N disks of different sizes which can slide to any tower.
    Tower of Hanoi pseudocode
    moveDisks(n: Int, origin: Tower, dest: Tower, buffer: Tower) {
    if (n <= 0) return
    moveDisks(n - 1, origin, buffer, destination)
    moveTop(origin, destination)
    moveDisks(n - 1, buffer, destination, origin)
  */

  //Permuations without dups: compute all permutations of a string of unique characters
  def insertCharAt(word: String, c: Char, i: Int): String = {
    word.substring(0, i) + c + word.substring(i)
  }

  def getPermutations(str: String): ArrayList[String] = {
    if (str == null) return null

    val perms = new ArrayList[String]()
    if (str.length == 0) {
      perms.add("")
      return perms
    }

    val first = str.charAt(0)
    //Get first char
    val remainder = str.substring(1)
    // remove first char
    //TODO: rewrite without creating substrings
    val words = getPermutations(remainder)
    val iter = words.iterator()
    while (iter.hasNext) {
      val word = iter.next()
      var j = 0
      while (j <= word.length) {
        val str = insertCharAt(word, first, j)
        perms.add(str)
        j += 1
      }
    }
    return perms
  }

  //Permutations with duplicates; Method to compute all permutations of a string whose
  //characters are not unique.
  //Algorithm: For each permutation generation we keep it in a hashMap so that the
  //duplicates can be avoided
  import java.util.HashMap
  def buildFreqTable(str: String): HashMap[Char, Int] = {
    val table = new HashMap[Char, Int]()
    str.foreach((c: Char) => {
      if (!table.containsKey(c)) table.put(c, 0)
      table.put(c, table.get(c) + 1)
    })
    return table
  }

  // prefix will get populated to the full string as we take it in
  // We start with prefix = ""
  // Then as we pick each charater out, we augment the prefix
  // prefix1 = "" + a + f( Map(a -> 1, b -> 1, c->2, d -> 2)
  // prefix2 = "" + b + f( Map(a -> 2, b -> 0, c -> 2, d -> 2)
  def getPermDuplicates(map: HashMap[Char, Int],
                        prefix: String,
                        remaining: Int,
                        result: ArrayList[String]): Unit = {
    // Base case: remaining becomes 0
    if (remaining == 0) {
      //prefix will continue to form the string using characters, if remaining = 0, we can
      //add it to the result
      result.add(prefix)
      return
    }

    val iter = map.keySet().iterator
    while (iter.hasNext) {
      val key = iter.next()
      val count = map.get(key)
      if (count > 0) {
        map.put(key, count - 1)
        getPermDuplicates(map, prefix + key, remaining - 1, result)
        // Restore the count prefix2 = "" + b + f( Map(a -> 2, b -> 0, c -> 2, d -> 2)
        map.put(key, count)
      }
    }
  }

  def getPermDuplicates(str: String): ArrayList[String] = {
    //generate a frequency table first and then use it to generate the permutations
    //Take an example like aaaaaaaa, it will slow down
    //another option is to count the permutation

    val result = new ArrayList[String]()
    val map = buildFreqTable(str)
    // Example str: abaccdd
    // Frequency map: Map('a' -> 2, 'b' -> 1, 'c' -> 2, 'd' -> 2)
    // Go over all the keys and then build respective
    // a + f( Map(a -> 1, b -> 1, c->2, d -> 2)
    // a here is the prefix
    getPermDuplicates(map, "", str.length, result)
    //TODO: Will it be str.length OR str.length - 1
    result
  }

  //Implement an algorithm to print all valid combination of n pair of parentheses
  //Input : n
  //We have a recursive function call paren(3, 3)
  // '(' + paren(2, 3)
  // '(' + '(' + '(' + paren(0, 3)
  def getParentheses(list: ArrayList[String],
                     validParentheses: Array[Char],
                     leftRem: Int,
                     rightRem: Int,
                     index: Int): Unit = {
    // This is the key invariant, rightRem should never be less than leftRem for well
    // formed parentheses
    if (leftRem < 0 || rightRem < leftRem) return

    //leftRem == 0 && rightRem == 0 we add the string to list
    if (leftRem == 0 && rightRem == 0) list.add(new String(validParentheses))
    else {
      validParentheses(index) = '('
      //Left recurse
      getParentheses(list, validParentheses, leftRem - 1, rightRem, index + 1)

      //TODO: I am not sure if this is index or index + 1
      validParentheses(index) = ')'
      getParentheses(list, validParentheses, leftRem, rightRem - 1, index + 1)
    }
  }

  def getParentheses(n: Int): ArrayList[String] = {
    // Each string is a valid combination of parentheses '(' + '(' + ')' + ')' + '(' + '('
    val valid = Array.fill[Char](2*n)(0x0)
    val list = new ArrayList[String]()
    getParentheses(list, valid, n, n, 0)
    return list
  }

  //Pain fill: given a screen, a point and a a colr, fill in the surrounding area till
  //the color changes from original color
  case class Color(r: Int, g: Int, b: Int)

  def paintFill(screen: Array[Array[Color]], r: Int, c: Int,
                ocolor: Color,
                ncolor: Color): Boolean = {
    if (r < 0 || r >= screen.length || c < 0 || c >= screen(0).length) return false

    if(screen(r)(c) == ocolor) {
      screen(r)(c) = ncolor
      paintFill(screen, r - 1, c, ocolor, ncolor) //up
      paintFill(screen, r + 1, c, ocolor, ncolor) //down
      paintFill(screen, r, c - 1, ocolor, ncolor) //left
      paintFill(screen, r, c + 1, ocolor, ncolor) //right
    }

    return true
  }

  def paintFill(screen: Array[Array[Color]], r: Int, c: Int, ncolor: Color): Boolean = {
    if (screen(r)(c) == ncolor) return false
    return paintFill(screen, r, c, screen(r)(c), ncolor)
  }

  //Given a infinite number of 25, 10, 5, 1 represent n cents
  //87 cents: 3 * 25 + f(87-75, Denomination = {10, 5, 1})
  //          8 * 10 + f(7, Denomination = {5, 1})

  //87 for example
  def makeChange(amount: Int, denoms: Array[Int], index: Int): Int = {
    if (index >= denoms.length - 1) return 1
    //last denomination
    val denomAmount = denoms(index)
    var ways = 0
    var i = 0
    while (i * denomAmount <= amount) {
      val amountRem = amount - i * denomAmount
      ways += makeChange(amountRem, denoms, index + 1)
      i += 1
    }
    return ways
  }

  def makeChange(n: Int) : Int = {
    val denoms = Array(25, 10, 5, 1)
    return makeChange(n, denoms, 0)
  }

  /*
   * Stack of boxes: There are stack of n boxes with width w1 x height h1 x depth d1
   * biggest stack that we can build is max(biggest stack with b_1, biggest stack with b_2,
   * ..., biggest stack with b_n)
   */
  case class Box(width: Int, height: Int, depth: Int) {
    def canBeAbove(box: Box): Boolean = {
      if (width > box.width && height > box.height && depth > box.depth) return true
      else return false
    }
  }

  import java.util.Arrays

  class BoxComparator extends Comparator[Box] {
    override def compare(x: Box, y: Box): Int = {
      // if same height then 0 and after that positive/negative
      return y.height - x.height
    }
  }

  def createStack(boxes: Array[Box], index: Int): Int = {
    val bottom = boxes(index)
    var maxHeight = 0
    var i = index + 1
    while (i < boxes.length) {
      if (boxes(i).canBeAbove(bottom)) {
        val height = createStack(boxes, i)
        maxHeight = Math.max(height, maxHeight)
      }
      i += 1
    }
    maxHeight += bottom.height
    return maxHeight
  }

  def createStack(boxes: Array[Box]): Unit = {
    Arrays.sort(boxes, new BoxComparator())
    var maxHeight = 0
    var i = 0
    while(i < boxes.length) {
      val height = createStack(boxes, i)
      maxHeight = Math.max(maxHeight, height)
      i += 1
    }
  }
}


