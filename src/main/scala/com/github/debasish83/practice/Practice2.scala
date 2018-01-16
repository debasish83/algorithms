package com.github.debasish83.practice

object Practice2 {

  import java.util.ArrayList

  //TODO: null can be replaced by Option
  class Node(val data: Char) {
    val children = new ArrayList[Node]()

    def getChild(c: Char): Node = {
      for (i <- 0 until children.size) {
        if (children.get(i).data == c) return children.get(i)
      }
      return null
    }

    override def toString(): String = {
      val buf = new StringBuilder
      buf.append(s"TrieNode $data\n")
      for (i <- 0 until children.size)
        buf.append(children.get(i).data)
      buf.toString()
    }
  }

  class Trie(dictionary: Array[String]) {
    var root: Node = new Node('*')
    dictionary.foreach(str => add(str))

    //TODO: This is not needed, we can mark a word bit on it
    val dictSet = dictionary.toSet

    //TODO: use an iterative algorithm
    def add(str: String, index: Int, iter: Node): Unit = {
      val c = str.charAt(index)
      var child = iter.getChild(c)
      if (child == null) {
        val node = new Node(c)
        iter.children.add(node)
        child = node
      }
      if (index + 1 < str.length) add(str, index + 1, child)
      else return
    }

    def add(str: String): Unit = {
      val index = 0
      add(str, index, root)
    }

    //TODO: Use a stack based iterative algorithm
    def find(prefix: String,
             iter: Node,
             patterns: ArrayList[String]): Unit = {
      if (iter.children.size == 0) return
      //be
      //bed is a valid word from dictionary
      //bedr is not a word
      //bedroom is a valid word from dictionary
      for (i <- 0 until iter.children.size) {
        val node = iter.children.get(i)
        val word = prefix + node.data
        if (dictionary.contains(word)) patterns.add(word)
        find(word, node, patterns)
      }
    }

    def find(str: String): ArrayList[String] = {
      var prefix = ""
      var iter = root

      for (i <- 0 until str.length) {
        val c = str.charAt(i)
        val node = iter.getChild(c)
        if (node != null) {
          prefix = prefix + node.data
          iter = node
        } else {
          //prefix did not match trie entries, return null
          return null
        }
      }

      //prefix = be
      println(s"prefix $prefix")
      val patterns = new ArrayList[String]()
      find(prefix, iter, patterns)
      patterns
    }
  }

  class AutoComplete(dictionary: Array[String]) {
    val trie = new Trie(dictionary)

    def find(str: String): ArrayList[String] = {
      trie.find(str)
    }
  }
}
