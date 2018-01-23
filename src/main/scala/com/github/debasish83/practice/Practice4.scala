package com.github.debasish83.practice

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, Set, Stack, StringBuilder}

object Practice4 {
  class Graph(words: Array[String]) {

    val adj = Map.empty[Int, Set[Int]]

    val min = 'a'.toInt

    class GraphException extends Exception

    for(i <- 0 until words.length) {
      val str = words(i)
      // Adding the edges in reverse order
      var k = str.length - 1
      while (k >= 1) {
        val toNode = str.charAt(k).toInt - min
        val fromNode = str.charAt(k - 1).toInt - min
        // Self nodes are not needed
        if (toNode != fromNode) addEdge(toNode, fromNode)
        k -= 1
      }
    }

    // It's a directed acyclic graph, n1 -> n2 will be in graph but not n2 -> n1
    // n1 and n2 are both vertices in the grah
    def addEdge(n1: Int, n2: Int): Unit = {
      if (adj.contains(n1)) {
        //Check if n2 is in the Set
        val adjacent = adj(n1)
        if (!adjacent.contains(n2)) {
          adjacent += n2
        }
      } else {
        adj.put(n1, Set(n2))
      }

      if (!adj.contains(n2)) {
        adj.put(n2, Set.empty[Int])
      }
    }

    def getNeighbors(id: Int): Iterator[Int] = {
      if (adj.contains(id)) {
        adj.get(id).get.toIterator
      } else {
        throw new Exception(s"$id not contained in the graph")
      }
    }

    override def toString(): String = {
      val sb = new StringBuilder()
      for (iter <- adj) {
        sb.append(iter._1)
        if (iter._2.size > 0) {
          sb.append(",")
          sb.append(iter._2.mkString(","))
        }
        sb.append("\n")
      }
      sb.toString()
    }

    def getNodes(): Iterator[Int] = {
      adj.keysIterator
    }

    def nodes(): Int = {
      adj.size
    }

    def topologicalSort(node: Int,
                        visited: Array[Boolean],
                        stack: Stack[Int]): Unit = {
      visited(node) = true
      val iter = getNeighbors(node)
      while (iter.hasNext) {
        val neighbor = iter.next()
        if (!visited(neighbor)) {
          topologicalSort(neighbor, visited, stack)
        }
      }
      stack.push(node)
    }

    def topologicalSort(): Array[Char] = {
      val visited = Array.ofDim[Boolean](26)
      val stack = new Stack[Int]()

      val iter = getNodes()
      while (iter.hasNext) {
        val node = iter.next()
        //If the node is not visited then start a sort with the specific node
        if (!visited(node)) {
          topologicalSort(node, visited, stack)
        }
      }
      val buf = new ArrayBuffer[Char]
      while (stack.isEmpty) {
        buf += (stack.pop() + min).toChar
      }
      buf.toArray
    }
  }

  import java.util.ArrayList

  //000101
  def getCidrs(str: String,
               num: Int,
               index: Int,
               patterns: ArrayList[String]): Unit = {
    var startIp = str
    val len = str.length
    var i = len - 1
    while (i > 0) {
      if (startIp.charAt(i).toInt == 1) {
        ???
      }
      i -= 1
      // New binary representation of IP
      startIp = ???
    }
    ???
  }

  def getCidrs(startIp: String, num: Int): Array[String] = {
    //127.0.0.4
    //127.0.0.5, 8
    //127.0.0.101 + 1
    //127.0.0.110 + 2
    //127.0.1.000 + 8
    //17 = 1 + 2^4
    //19 = 1 + 2 + 2^4
    //
    //startIp comes in generate
    val ipBlocks = startIp.split(".")
    val buf = new mutable.StringBuilder()
    ipBlocks.foreach { ip =>
      buf.append(ip.toInt.toBinaryString)
    }
    val patterns = new ArrayList[String]()
    val index = 0
    getCidrs(buf.toString, num, index, patterns)
    ???
  }
}
