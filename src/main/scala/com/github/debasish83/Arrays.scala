package com.github.debasish83

/**
  * Created by v606014 on 12/7/17.
  */
object Arrays {

  /*
    Even Rows

    [1 4 5]
    [2 3 4]
    [4 5 6]
    [3 4 5]

    Odd Rows

    [1 4 5]
    [2 3 4]
    [4 5 6]

    layer = 0, 1
   */
  //printClockwise: (0, 0) -> (0, maxCol) -> (maxRow, maxCol) -> (maxRow, 0) -> (0, 0)
  // layer = 0, layer = 1
  // (1, 1) -> (1, maxCol - 1) -> (maxRow - 1, maxCol - 1) -> (maxRow - 1, 1) -> (1,1)

  def printClockwise(matrix: Array[Array[Int]]): Unit = {
    val maxRows = matrix.length - 1
    val maxCols = matrix(0).length - 1

    var layer = 0
    while(layer < maxRows/2) {
      var row = layer
      var col = layer
      while(col < maxCols - layer) {
        println(matrix(row)(col))
        col += 1
      }
      //row=layer, col = maxCols - layer
      while(row < maxRows - layer) {
        println(matrix(row))
      }
      layer += 1
    }
    ???
  }
  /*
    [1 4 5]
    [2 3 4]
    [4 5 6]
    [3 4 5]

    1
    4 2
    5 3 4
    4 5 3
    6 4
    5
   */

  // row = 0, .., maxRow - 1
  // row = 0 (0, 0) 1
  // row = 1 (1, 0) (0, 1)
  // row = 2 (2, 0) (1, 1) (0, 2)
  // row = 3 (3, 0) 3 (2, 1) 5 (1, 2) 4 hit maxCol

  // col = 0 hit maxRow
  // col = 1
  // col = 2
  def printDiagonal(matrix: Array[Array[Int]]): Unit = {
    val maxRow = matrix.length
    val maxCol = matrix(0).length

    var row = 0
    while (row < maxRow) {
      var i = row
      //row = 0
      var j = 0
      while (i >= 0 && j < maxCol) {
        println(matrix(i)(j))
        j += 1
        i -= 1
      }
      row += 1
    }
  }


  // If an element in an matrix is 0, make entire row/column to 0
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val rowMarker = Array.fill[Int](matrix.length)(0)
    val colMarker = Array.fill[Int](matrix.length)(0)

    var i = 0
    var j = 0
    while (i < matrix.length) {
      while (j < matrix.length) {
        if (matrix(i)(j) == 0) {
          rowMarker(i) = 1
          colMarker(i) = 1
        }
        j += 1
      }
      i += 1
    }
    i = 0
    j = 0
    while (i < matrix.length) {
      while (j < matrix.length) {
        if (rowMarker(i) == 1 || colMarker(j) == 1) matrix(i)(j) = 0
        j += 1
      }
      i += 1
    }
  }

  // Given two string one is rotation of other
  // s1.concat(s1).substring(s2)

  // Rotate matrix
  // N x N input[][]
  // if it is 5 x 5 then layer goes from 0 till 5/2 = 2
  // if it is 4 x 4 then layer goes from 0 till 4/2 = 2
  case class Pixel(r: Int, g: Int, b: Int)
  def rotate(image: Array[Array[Pixel]]): Unit = {
    val n = image.length
    var layer = 0
    while(layer < n/2) {
      val first = layer
      val last = n - 1 - layer
      var i = first
      // image(0)(0) -> image(0)(n-1)
      // image(0)(n-1) -> image(n-1)(n-1)

      // image(1)(1) -> image(1)(n - 2)
      // image(1)(n-2) -> image(n-2)(n-2)
      while(i < last) {
        val offset = i - first
        val top = image(first)(i)
        image(first)(i) = image(last - offset)(first)
        image(last - offset)(first) = image(last)(last - offset)
        image(last)(last - offset) = image(i)(last)
        image(i)(last) = top
        i += 1
      }
    }
  }

  // Print the matrix in clockwise fashion, RobinHood phone screen
  class Visitor[T] {
    def visit(element: T): Unit = {
      println(element)
    }
  }

  /*
   There are two kind of matrices
   image: Array[Array[Int]]

   image(0)(0), ..., image(0)(n-1)

   We are increasing x coordinates here

   image(0)(0)
   image(1)(0)
   image(2)(0)
   ...
   will increase the y coordinates

   Now let's see the matrices with even/odd max_y

   max_y = 3 layer = 0, ceil(max_y/2) = 2

   odd-y, even-x
   (0,0) (0,1) (0,2) (0,3)
   (1,0) (1,1) (1,2) (1,3)
   (2,0) (2,1) (2,2) (2,3)

   Print from (0,0) till (1,0) and then go to (1,1)

   odd-y, odd-x
   (0,0) (0,1) (0,2)
   (1,0) (1,1) (1,2)
   (2,0) (2,1) (2,2)

   Print from (0) till (1,0) and then go to (1,1)

   Now if it is even matrices then we have the following configs

   even-y, even-x
   (0,0) (0,1) (0,2) (0,3)
   (1,0) (1,1) (1,2) (1,3)
   (2,0) (2,1) (2,2) (2,3)
   (3,0) (3,1) (3,2) (3,3)

   layer = 0 .. ceil(4/2) = 2
   (0,0) -> (1,0)
   (1,1) -> (2,1)

   even-y, odd-x
   (0,0) (0,1) (0,2)
   (1,0) (1,1) (1,2)

   layer = 0, .., 1
   Go from (0,0) -> (1,0)

   These 4 scenerios cover all matrix possibility
   */
  def visitClockwise(image: Array[Array[Int]]): Unit = {
    // TODO: Add checks if it's not a valid matrix
    val maxx = image(0).length
    val maxy = image.length

    val visitor = new Visitor[Int]
    // ceil to handle the old and even cases
    val numLayers = math.ceil(maxy/2.0).toInt

    println(s"total layers $numLayers")

    var layer = 0

    while(layer < numLayers) {
      var i = layer
      var j = layer

      // Increment x direction
      // i = layer, j = layer, ..., maxx - layer - 1
      while (j < maxx - layer - 1) {
        visitor.visit(image(i)(j))
        j += 1
      }

      // Increment y direction
      while (i < maxy - layer - 1) {
        visitor.visit(image(i)(j))
        i += 1
      }

      // Decrement x direction
      while (j > layer) {
        visitor.visit(image(i)(j))
        j -= 1
      }

      // Decrement y direction
      // i = maxy - layer - 1, .. , layer, j = layer
      // i = 3 i > 0 + 1 = 1
      // 3, 2 => 2 , 1
      while (i > layer) {
        visitor.visit(image(i)(j))
        i -= 1
      }
      println(s"completed layer $layer $i $j")
      layer += 1
    }
  }

  //BigNumber a(Array(2, 4, 5, 6))
  //a(i) : how to override (i) ?
  case class BigNumber(elems: Array[Int]) {
    override def toString(): String = {
      elems.mkString("")
    }

    def get(i: Int): Int = {
      elems(elems.length - i - 1)
    }
  }

  import java.util.Stack

  class Calculator {
    def add(a: BigNumber, b: BigNumber): BigNumber = {
      // a.length b.length
      val result = new Stack[Int]

      val lena = a.elems.length - 1
      val lenb = b.elems.length - 1

      val minLength = Math.min(lena, lenb)

      val max = if (minLength == lena) b else a

      // 2 3 4 2 + 3 1 7 2

      // 2 + 2 sum = 4 carry = 0
      // 4 + 7 sum = (11 + carry) % 10
      // sum = (a(i) + b(i) + carry) / 10
      // carry = (a(i) + b(i) + carry) % 10

      // 5 2 3 4 2 + 3 1 7 2

      //minLength = 3

      // max = 5
      // min = 4

      var carry = 0
      var i = 0
      while (i <= minLength) {
        val sum = a.get(i) + b.get(i) + carry
        result.add(sum / 10)
        carry = sum % 10
        i += 1
      }

      // i = 3

      val maxLength = max.elems.length - 1

      while (i <= maxLength) {
        val sum = max.get(i) + carry
        result.add(sum / 10)
        carry = sum % 10
        i += 1
      }

      if (carry > 0) result.add(carry)

      val out = Array.fill[Int](result.size)(0)
      i = 0
      while (result.empty()) {
        out(i) = result.pop()
        i += 1
      }
      return new BigNumber(out)
    }
  }

  def main1(args: Array[String]): Unit = {
    println("test")
    args(0) match {
      case "clockwise" =>
        println("odd odd square example1")
        val image1 = Array(
          Array(3, 2, 4),
          Array(5, 1, 2),
          Array(4, 2, 5))
        visitClockwise(image1)

        println("odd odd rectangular")
        val image4 = Array(
          Array(3, 2, 4, 1, 2),
          Array(5, 1, 2, 4, 2),
          Array(4, 2, 5, 6, 1))
        visitClockwise(image4)

        println("odd odd square example2")
        val image5 = Array(
          Array(3, 2, 4, 1, 2),
          Array(5, 1, 2, 4, 2),
          Array(4, 2, 5, 6, 1),
          Array(5, 1, 2, 4, 2),
          Array(3, 2, 4, 1, 2))
        visitClockwise(image5)

        println("odd even")
        val image2 = Array(
          Array(3, 2, 3, 4),
          Array(5, 1, 2, 2),
          Array(4, 2, 4, 5))
        visitClockwise(image2)

        println("even even")
        val image3 = Array(
          Array(3, 2, 3, 4),
          Array(5, 1, 2, 2),
          Array(4, 2, 4, 5),
          Array(2, 4, 5, 3)
        )
        visitClockwise(image3)

      case _ => println("unknown command")
    }
  }

  def main2(args: Array[String]): Unit = {
    val matrix =
      Array(
        Array(1, 4, 5),
        Array(2, 3, 4),
        Array(4, 5, 6),
        Array(3, 4, 5))
    printDiagonal(matrix)
  }
}

