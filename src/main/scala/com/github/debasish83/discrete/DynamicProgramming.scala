package com.github.debasish83.discrete

import com.github.debasish83.discrete.Recursive.{Box, Point, BoxComparator}

/**
  * @author by v606014 on 11/15/17.
  */

//Dynamic programming algorithms mainly focus on taking a recursive algorithm and find
//the overlapping subproblems which are repeated calls. After that we can cache them for
//future recursive calls
object DynamicProgramming {
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) return n

    val memo = Array.fill[Int](n)(0)

    var a = 0
    var b = 1

    //We can clean the whole memoization array
    //memo(0) = 0
    //memo(1) = 1

    var i = 2
    while (i < n) {
      val c = a + b
      a = b
      b = c
      //memo(i) = memo(i - 1) + memo(i - 2)
      i += 1
    }
    return a + b
    //return memo(n - 1) + memo(n - 2)
  }

  //getPath algorithm starts with r and c and look for adjacent coordinates (r-1, c)
  //(r, c-1). If square is offlimit we ignore it. Then it's next set of search points
  //(r-1, c) => (r-1, c-1) (r-2, c)
  //(r, c - 1) => (r-1, c-1) (r, c-2)
  //(r-1, c-1) computation should get re-used
  import java.util.ArrayList
  import java.util.HashSet

  def getPath(matrix: Array[Array[Boolean]], row: Int, col: Int,
              path: ArrayList[Point], failed: HashSet[Point]): Boolean = {
    if (row < 0 || col < 0 || !matrix(row)(col)) return false

    val p = Point(row, col)
    if (failed.contains(p)) return false

    val isOrigin = (row == 0) && (col == 0)

    if (isOrigin ||
      getPath(matrix, row, col - 1, path, failed) ||
      getPath(matrix, row - 1, col, path, failed)) {
      path.add(p)
      return true
    }
    failed.add(p)
    return false
  }

  def getPath(matrix: Array[Array[Boolean]]): ArrayList[Point] = {
    if (matrix == null || matrix.length == 0) return null
    val path = new ArrayList[Point]()
    val failed = new HashSet[Point]()
    if (getPath(matrix, matrix.length - 1, matrix(0).length - 1, path, failed)) {
      return path
    }
    return null
  }

  import java.util.HashMap

  def makeChange(amount: Int, denoms: Array[Int],
                 index: Int,
                 map: HashMap[Int, Array[Int]]): Int = {
    if (map.containsKey(amount)) {
      val cachedWays = map.get(amount)(index)
      if (cachedWays > 0) return cachedWays
    }

    if (index >= denoms.length - 1) return 1 // one denom remaining

    val denomAmount = denoms(index)

    var ways = 0
    var i = 0
    while (i * denomAmount <= amount) {
      val amountRem = amount - i * denomAmount
      ways += makeChange(amountRem, denoms, index + 1, map)
      i += 1
    }
    //Cache the ways computation
    if (!map.containsKey(amount)) map.put(amount, Array.fill[Int](4)(0))
    map.get(amount)(index) = ways
    return ways
  }

  def makeChange(n: Int): Int = {
    val denoms = Array(25, 10, 5, 1)
    //hashMap that maps from amount to a new hash table
    //The new hash table maps from denom to the precomputed values
    //map(87)(0) = n, how many ways to represent 87 cents with 25 cents
    //hashMap(amount) = Array[4]
    val map = new HashMap[Int, Array[Int]]()
    return makeChange(n, denoms, 0, map)
  }

  //8 queen problem: algorithm to arrange 8 queens on a board so that they don't share
  //same row, column or diagonal
  //Idea is that we put queen at one position and find all other possible positions so that
  //row, column and diagonal constraints are met

  // We generate a feasible column vector which is added to the list of results once we
  // traverse through all the rows

  // Why don't we need to store the whole 8 x 8 board but only a column vector
  def checkValid(columns: Array[Int],
                 row: Int,
                 col: Int): Boolean = {
    //row and col are possible placement of the new queen
    //We have to check if rest of queen that are placed in the columns are not
    //causing the constraint violation
    var row2 = 0
    while (row2 < row) {
      val col2 = columns(row2)
      if (col == col2) return false // Violation on row and column positions

      val colDist = Math.abs(col2 - col)
      val rowDist = row - row2
      if (colDist == rowDist) return false //For diagonal check \delta row = \delta col
      row2 += 1
    }
    return true
  }

  def placeQueens(row: Int,
                  columns: Array[Int],
                  results: ArrayList[Array[Int]]): Unit = {
    if (row == 8) results.add(columns.clone())
    else {
      var col = 0
      while (col < 8) {
        if (checkValid(columns, row, col)) {
          columns(row) = col //Place queen
          placeQueens(row + 1, columns, results)
        }
        col += 1
      }
    }
  }

  import java.util.Arrays

  def createStack(boxes: Array[Box], index: Int, stackMap: Array[Int]): Int = {
    // Use the cached maxHeight for stackMap if available
    if (index < boxes.length && stackMap(index) > 0) {
      return stackMap(index)
    }
    val bottom = boxes(index)
    var maxHeight = 0
    var i = index + 1
    while (i < boxes.length) {
      if (boxes(i).canBeAbove(bottom)) {
        val height = createStack(boxes, i, stackMap)
        maxHeight = Math.max(height, maxHeight)
      }
      i += 1
    }
    maxHeight += bottom.height
    stackMap(index) = maxHeight
    return maxHeight
  }

  // The createStack is in-efficient since we are recomputing the maxHeight
  // At each index, we should cache the maxHeight
  def createStack(boxes: Array[Box]): Int = {
    Arrays.sort(boxes, new BoxComparator())
    val stackMap = Array.fill[Int](boxes.length)(0)
    var maxHeight = 0
    var i = 0
    while (i < boxes.length) {
      val height = createStack(boxes, i, stackMap)
      maxHeight = Math.max(maxHeight, height)
      i += 1
    }
    return maxHeight
  }

  //Given a boolean expression consisting of symbols 0 (false), 1 (true), &, |, ^ (XOR)
}