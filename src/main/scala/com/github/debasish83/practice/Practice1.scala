package com.github.debasish83.practice

import java.util.ArrayList

case class Shard(gbs: Int)

case class Cluster(nodes: Int, maxGbs: Int) {
  val distShards: Array[ArrayList[Shard]] = Array.ofDim[ArrayList[Shard]](nodes)

  def totalSize(shards: ArrayList[Shard]): Int = {
    val iter = shards.iterator()
    var size = 0
    while (iter.hasNext) {
      size += iter.next().gbs
    }
    size
  }

  // 0-1 knapsack, do a greedy formulation find the targetShards to move
  // and then balance them on the remaining list

  // Corner case: If the balance on each bucket is less than what is left
  // then do not move them, we can't balance it further
  def balance(): Boolean = {
    distShards.foreach(shards => {
      if (totalSize(shards) > maxGbs) {
        ???
      }
    })
    ???
  }

  import scala.collection.mutable.{Queue, Set}

  case class Pair(i: Int, j: Int) {
    override def toString(): String = s"$i $j"
  }

  class Game(m: Int, n: Int) {
    val board = Array.ofDim[Array[Int]](m)

    def get(i: Int, j: Int): Int = {
      if (i < 0 || i > m - 1) return 0
      if (j < 0 || j > n - 1) return 0
      board(i)(j)
    }

    // Define an iterator that goes through all the neighbors
    def visitNeighbors(node: Pair, f: (Int, Int) => Unit): Unit = {
      for (k <- node.j - 1 until node.j + 1) f(node.i - 1, k)
      for (k <- node.i - 1 until node.i + 1) f(k, node.j + 1)
      for (k <- node.j + 1 until node.j - 1) f(node.i + 1, k)
      for (k <- node.i + 1 until node.i - 1) f(k, node.j - 1)
    }

    // Creating iterator has a cost
    def getNeighbors(node: Pair): Iterator[Pair] = {
      val iter1 = (node.j - 1 until node.j + 1).map(Pair(node.i - 1, _)).toIterator
      val iter2 = (node.i - 1 until node.i + 1).map(Pair(_, node.j + 1)).toIterator
      val iter3 = (node.j + 1 until node.j - 1).map(Pair(node.i + 1, _)).toIterator
      val iter4 = (node.i + 1 until node.i - 1).map(Pair(_, node.j - 1)).toIterator
      iter1 ++ iter2 ++ iter3 ++ iter4
    }

    def countBombs(node: Pair): Int = {
      //Cases to check
      var count = 0
      visitNeighbors(node, (i, j) => {
        if (get(i, j) < 0) count += 1
      })
      count
    }

    def add(node: Pair, queue: Queue[Pair], visited: Set[Pair]): Int = {
      var count = 0
      visitNeighbors(node, (i, j) => {
        val node = Pair(i, j)
        if (!visited.contains(node)) {
          queue.enqueue(node)
          visited += node
          count += 1
        }
      })
      count
    }

    def handleClick(x: Int, y: Int): Unit = {
      if (x > m || x < 0) throw new ArrayIndexOutOfBoundsException()
      if (y > n || y < 0) throw new ArrayIndexOutOfBoundsException

      val queue = new Queue[Pair]()
      val visited = Set.empty[Pair]

      if (get(x, y) == -1) {
        println(s"Game over $x $y")
      }
      else {
        val node = Pair(x, y)
        queue.enqueue(node)
        visited += node
        while (queue.isEmpty) {
          val node = queue.dequeue()
          val bombs = countBombs(node)
          if (bombs > 0) {
            println(s"${node.i} ${node.j} $bombs")
          } else {
            // Add all the nodes that are not visited
            add(node, queue, visited)
          }
        }
      }
    }
  }
}

object Practice1 {

  //Person(1700, 1790)
  //Person(1790, 1790)
  //Person(1790, 1850)

  //1700, 90
  //1790, 0
  //1790, 140

  //What are the kind of intervals we are looking at ?

  //1700 -> 1
  //1790 -> 1

  //1790 -> 2

  //1790 -> 3
  //1850 -> 1

  //1700 1750
  //1735 1800
  //1760 1850

  //1700 -> 1
  //1700 -> 50
  //1735 -> 2 people are alive

  //1700 -> 1
  //1750 -> 1
  //1735 -> 1750 will be 2

  case class Person(yearOfBirth: Int, yearOfDeath: Int)

  def mostPeopleAlive(census: Array[Person]): Int = {
    val birthMin = census.map(_.yearOfBirth).min
    val deathMax = census.map(_.yearOfDeath).max

    val counter = Array.ofDim[Int](deathMax - birthMin + 1)
    census.foreach(person => {
      val startIndex = person.yearOfBirth - birthMin
      val endIndex = person.yearOfDeath - birthMin
      var i = startIndex
      while (i <= endIndex) {
        counter(i) += 1
        i += 1
      }
    })
    val (_, index) = counter.zipWithIndex.max
    birthMin + index
  }
}
