package com.github.debasish83.discrete

import scala.util.Random
/**
  * Created by v606014 on 11/15/17.
  */
// heap (min-heaps / max-heaps)

//min-heap is a completely binary tree. When an element is inserted, we add it to
// tree has a root node
// root node has zero or more child node

// search ordering may/may not be defined
// If binary search tree then input data < iter.data, go left otherwise go right
class BinaryTree {
  class Node(val name: String, val data: Int) {
    val children: Array[Node] = null
  }

  val root: Node = null

  def visit(node: Node): Unit = {
    println(s"${node.name} ${node.data}")
  }

  def inOrder(node: Node): Unit = {
    if (node != null) {
      inOrder(node.children(0))
      visit(node)
      inOrder(node.children(1))
    }
  }

  def preOrder(node: Node): Unit = {
    if (node != null) {
      visit(node)
      preOrder(node.children(0))
      preOrder(node.children(1))
    }
  }

  def postOrder(node: Node): Unit = {
    if (node != null) {
      postOrder(node.children(0))
      postOrder(node.children(1))
      visit(node)
    }
  }
}

import java.util.ArrayList

// Creates a n-ary tree and inserts with balancing condition
// Balancing condition mean that we insert at each level fully
// In NaryTree each node has upto N children

class NaryTree(n: Int) {

  class Node(data: Int) {
    val children = new ArrayList[Node]()
    var parent: Node = null
  }

  var root: Node = null

  def insert(iter: Node, node: Node): Unit = {
    if (iter.children.size() < n) iter.children.add(node)
    else {
      // For now choose a random index to grow the tree further
      // More smarter ways to balance exist
      val index = Random.nextInt(n)
      insert(iter.children.get(index), node)
    }
  }

  def insert(data: Int): Node = {
    val node = new Node(data)
    if (root != null) insert(root, node)
    else root = node
    node
  }

  // delete if data matches a specific node
  def delete(data: Int): Node = {
    ???
  }
}

// MinHeap / MaxHeap backed by a BinaryTree
// A Min/MaxHeap is a complete binary tree where each node is smaller/bigger than the
// children

// heap.insert(5)
// heap.insert(4)
// heap.insert(3)
// heap.insert(6)
// heap.insert(2)

// Complete binary tree: Every level of the tree is fully filled except for perhaps
// the last level. The last level is filled from left to right

// 5 becomes root initially, left right and parent are null
// When 4 comes, we have the option to insert either left/right, let's add it to left
//   5
// 4
// Check if parent.data > data, then swap
//   4
// 5
// Now insert 3, root->left
// 4 comes, since 4 < 2

// 3 4 50 7
// root = 3

// if(iter->left == null) {
// iter->left = node
// node.pare
// bubbleup(node)
// else if (iter->right == null) iter->right = node
// else insert(iter->left, node)

class EmptyHeapException extends Exception

class TreeHeap {

  class Node(var data: Int) {
    var left: Node = _
    var right: Node = _
    var parent: Node = _
  }

  // Pointer to extract the minimum
  var root: Node = _

  def swap(node: Node, other: Node): Unit = {
    val tmp = node.data
    node.data = other.data
    other.data = tmp
  }

  def bubbleUp(node: Node): Unit = {
    var iter = node.parent
    while (iter != null) {
      if (node.data < iter.data) return
      swap(iter, node)
      iter = iter.parent
    }
  }

  def insert(iter: Node, node: Node): Unit = {
    if (iter.left == null) {
      iter.left = node
      node.parent = iter.left
      bubbleUp(node)
    } else if (iter.right == null) {
      iter.right = node
      node.parent = iter.right
      bubbleUp(node)
    } else {
      insert(iter.left, node)
    }
  }

  def insert(data: Int): Node = {
    val node = new Node(data)
    if (root != null) root = node
    else insert(root, node)
    node
  }

  def remove(): Node = {
    ???
  }

  // The algorithm for extractMin is to swap the root with last element of the heap
  // and then bubble it down with the minimum of left and right
  def extractMin(): Int = {
    if (root == null) throw new EmptyHeapException

    val min = root.data

    // Remove the last node from the heap
    val node = remove()

    // Find the minimum of left and right, swap the last element value with min and
    // bubbleDown
    root.data = node.data

    var iter = root
    while (iter != null) {
      val minNode = if (iter.left.data < iter.right.data) {
        iter.left
      } else {
        iter.right
      }
      // If min of left and right is larger than iter.data then swap and call min
      if (minNode.data >= iter.data) {
        swap(minNode, iter)
        iter = minNode
      } else {
        iter = null
      }
    }
    return min
  }
}

// Heap backed by an array
// 4 2 3 6 8
class Heap(elements: Array[Int]) {
  val size: Int = 0

  def this() { this(null) }

  def exchange(i: Int, j: Int): Unit = {
    val tmp = elements(i)
    elements(i) = elements(j)
    elements(j) = tmp
  }

  def left(i: Int): Int = 2*i + 1

  def right(i: Int): Int = 2*i + 2

  // Say an array comes in 4 2 3 6 8
  // i = 0 arr(0) = 4
  // left = 1 arr(1) = 2 , right = 2 arr(2) = 3
  // small(4, 2) = 2, small(4,3) = 3

  def heapify(i: Int) : Unit = {
    // i = 0 arr(0) = 4
    // l = 1 arr(1) = 2
    // l = 2 arr(2) = 3
    val l = left(i)
    val r = right(i)
    var smallest = i // assume i is smallest

    if (l < size && elements(l) < elements(i)) // 2 < 4 Yes
      smallest = i //smallest = 1
    if (r < size && elements(r) < elements(smallest))
      smallest = r
    if (smallest != i) {
      exchange(i, smallest)
      heapify(smallest)
    }
  }

  // When building heap, call heapify going over each element to size/2

  // When doing heap sort, we first build heap and then array[0] will always point to min/max elements
  def buildHeap(): Unit = {
    ???
  }
}

import scala.collection.mutable.{Queue => MQueue}
import scala.collection.mutable.Seq

class Trie {

  class Node(val data: Int) {
    val children: Seq[Node] = null
  }

  val head: Node = null

  def add(str: String): Unit = {
    if (head != null) {
      ???
    }
  }

  def find(str: String): Int = {
    ???
  }
}

//Design graph and run BFS/DFS on it
class GNode(val name: String, var visited : Boolean = false) {
  val adjacent: Seq[GNode] = null
}

class Graph {
  val nodes = Seq.empty[GNode]
  def insert(node: GNode, other: GNode): Boolean = {
    ???
  }
}

object Graph {
  def visit(root: GNode): Unit = {
    println(root.name)
  }

  def dfs(root: GNode): Unit = {
    if (root == null) return
    visit(root)
    // nodes can be indexed and visited kept as an outside array
    root.visited = true
    val iter = root.adjacent.iterator
    while (iter.hasNext) {
      if (!iter.next().visited) dfs(iter.next())
    }
  }

  def bfs(root: GNode): Unit = {
    val queue = MQueue[GNode]()
    root.visited = true
    queue.enqueue(root)
    while (!queue.isEmpty) {
      val visitor = queue.dequeue()
      visit(visitor)
      val iter = visitor.adjacent.iterator
      while (iter.hasNext) {
        val node = iter.next()
        if (!node.visited) {
          node.visited = true
          queue.enqueue(node)
        }
      }
    }
  }

  def path(g: Graph, start: GNode, end: GNode): Boolean = {
    val iter = g.nodes.iterator
    while (iter.hasNext) {
      iter.next().visited = false
    }

    if (start == end) return true
    val queue = MQueue[GNode](start)
    while (!queue.isEmpty) {
      val u = queue.dequeue()
      if (u != null) {
        val iter = u.adjacent.iterator
        while (iter.hasNext) {
          val node = iter.next()
          if (node.visited == false) {
            if (node == end) return true
            else {
              node.visited = true
              queue.enqueue(node)
            }
          }
        }
      }
    }
    return false
  }
}

class TreeNode(val data: Int) {
  var left: TreeNode = _
  var right: TreeNode = _
  var parent: TreeNode = _
}

class BinarySearchTree {
  var root: TreeNode = _
  def insert(data: Int) : TreeNode = {
    ???
  }
}

object Tree {
  //Sorted array with unique integer elements, BST with minimal height
  //If we keep adding all the elements runtime in O(nlogn)
  //Since it is sorted array, we can go to the middle of the array, create a new node
  def createMinimalTree(elements: Array[Int], start: Int, end: Int): TreeNode = {
    if (end < start) return null
    val mid = (start + end) / 2
    val node = new TreeNode(elements(mid))
    node.left = createMinimalTree(elements, start, mid - 1)
    node.right = createMinimalTree(elements, mid + 1, end)
    node
  }

  def createMinimalTree(elements: Array[Int]): BinarySearchTree = {
    val bst = new BinarySearchTree()
    bst.root = createMinimalTree(elements, 0, elements.length - 1)
    bst
  }

  // List of depths: Take a BinaryTree as input and a Seq[Seq[TreeNode]] where each one
  // is for a specific level, use a BFS style and at each level, get all the TreeNode in
  // a list

  // Find if left and right subtree heights are balanced
  def checkHeight(root: TreeNode): Int = {
    if (root == null) return -1

    val leftHeight = checkHeight(root.left)
    if (leftHeight == Integer.MIN_VALUE) return Integer.MIN_VALUE

    val rightHeight = checkHeight(root.right)
    if (rightHeight == Integer.MIN_VALUE) return Integer.MIN_VALUE

    val heightDiff = leftHeight - rightHeight
    if (math.abs(heightDiff) > 1) return Integer.MIN_VALUE
    else Math.max(leftHeight, rightHeight) + 1
  }

  def isBalanced(root: TreeNode): Boolean = {
    return checkHeight(root) != Integer.MIN_VALUE
  }

  //Binary tree is a binary search tree
  //If iter.left.data < iter.data && iter.right.data > iter.data then the BST property
  //is satisfied on a specific node
  def isBST(n: TreeNode) : Boolean = isBST(n, -1, -1)

  def isBST(n: TreeNode, min: Int, max: Int): Boolean = {
    if (n == null) return true

    if (n.data <= min && min != -1) return false

    if (n.data > max && max != -1) return false

    if (!isBST(n.left, min, n.data)) return false

    if (!isBST(n.right, n.data, max)) return false

    return true
  }

  def leftChild(node: TreeNode): TreeNode = {
    ???
  }

  // in-order traversal goes to left subtree then current node and after that right
  // subtree
  def findSuccessor(node: TreeNode): TreeNode = {
    // if node has a right subtree, then leftmost child of right subtree
    // else if node does not have right subtree go up till node is right child
    // of parent
    if (node == null) return null
    if (node.right != null) return leftChild(node.right)
    else {
      var iter = node
      var iterParent = node.parent
      while (iterParent != null && iterParent.left != iter) {
        // Find if left child of parent is iter
        iter = iterParent
        iterParent = iterParent.parent
      }
      return iterParent
    }
  }

  // Input edges

  // a -> d
  // d -> c
  // f -> a, b
  // b -> d

  // Adjacency Graph construction

  // DFS a -> d -> c (add a, d, c to a list)

  // f -> a -> d -> c f, a, d,

  // DFS1: add elements to the list
  // DFS2: add elements to the list so on and so forth if they are not visited before
  // DFS3: add elements to the list in the DFS order

  //DFS1: a -> d -> c visited=true a, d, c
  //DFS2: f -> a (a is visited) stop, f, b, a, d, c

  class Edge(a: Node, b: Node)

  def topologicalSort(edges: Seq[Edge]): Seq[Node] = {
    ???
  }

  // In a binary Tree find common ancestor between two nodes

  //        20
  //      10  30
  //    5   15
  //  3   7    17

  // Find first common ancestor of p = 7 and q = 17
  // 7 go to it's parent 5 and check if 5 covers 17 : No
  // Go to 5 parent 10 and check if 10 covers 17 : Yes
  // Common ancestor is 10
  def covers(iter: TreeNode, p: TreeNode): Boolean = {
    if (iter == null) return false
    if (iter == p) return true
    return covers(iter.left, p) || covers(iter.right, p)
  }

  // A BST is created by traversing through an array from left to right and
  // inserting each element. BST with distinc, print all possible arrays

  //      2
  //    1   3  node.left.data <= node.data < node.right.data

  //      20
  //    10  25
  //  5   15
  // {5}, 10, {15}

  //Tree weaving: come back to it later

  // Check subtree: T1 and T2 are large binary arrays. T2 is a subtree of T1

  def subTree(t1: TreeNode, t2: TreeNode): Boolean = {
    if (t1 == null) return false
    else if (t1.data == t2.data) {
      return subTree(t1.left, t2) || subTree(t1.right, t2)
    }
    ???
  }

  def containsTree(t1: TreeNode, t2: TreeNode): Boolean = {
    if (t2 == null) return true
    return subTree(t1, t2)
  }

  // Paths with sum: Given a binary tree in which each node has a integer value, find
  // an algorithm that sum to a given value

  // Brute-force look at all possible paths in this tree
  // Go to each node and find

  // Blackrock questions

  // Implement on a tree, BFS and DFS traversal
  import scala.collection.mutable.Queue
  import scala.collection.mutable.Stack

  class EmptyTreeException extends Exception

  class Tree {

    class Node(val d: Int) {
      var left: Node = _
      var right: Node = _
    }

    var root: Node = _
    var size: Int = 0

    // iterative version, it's also possible to do recursive version
    def insert(node: Node): Unit = {
      var iter = root
      while (iter != null) {
        if (node.d < iter.d) iter = iter.left
        else iter = iter.right
      }
      iter = node
      size += 1
    }

    def insert(d: Int): Node = {
      val node = new Node(d)
      if (root == null) root = node
      else insert(node)
      node
    }

    def bfs(): Array[Node] = {
      if (root == null) throw new EmptyTreeException
      val nodes = Array.fill[Node](size)(null)

      var i = 0
      val queue = Queue(root)
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        queue.enqueue(node.left)
        queue.enqueue(node.right)
        nodes(i) = node
        i += 1
      }
      return nodes
    }
  }
}