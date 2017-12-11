package com.github.debasish83.discrete

/**
  * @author debasish83 on 11/14/17.
  */
class Node(var data: Int) {
  var next: Node = _
}

class LinkedList {
  var head: Node = null

  def add(data: Int): Node = {
    val end = new Node(data)
    var iter = head
    while (iter.next != null) {
      iter = iter.next
    }
    iter = end
    iter
  }

  /*
   * Find if the data exists in linked list and delete it
   */
  def delete(d: Int): Boolean = {
    if (head.data == d) {
      head = null
      return true
    }

    var iter = head
    while (iter.next != null) {
      if (iter.next.data == d) {
        iter.next = iter.next.next
        return true
      }
      iter = iter.next
    }
    return false
  }

  // Run a pointer 2x the other, if they meet there is a cycle
  def findCycle(): Boolean = {
    ???
  }
}

object LinkedListHelper {

  // External storage: Add a HashMap that keep duplicates and delete the references
  // No storage: one iter and one runner, at each iter we see the data and runner find the data and removes
  def deleteDuplicates(ll: LinkedList): Unit = {
    if (ll.head == null) return
    var iter = ll.head
    while(iter.next != null) {
      val data = iter.data
      var runner = iter
      while (runner.data != data) {
        runner = runner.next
      }
      if (runner != null) {
        runner.next = runner.next.next
      }
      iter = iter.next
    }
  }

  // Kth to last element, If total size is n then n - k element
  // list of size 10, find 2nd to last element
  // Recursive
  case class Counter(var value: Int)

  def findKthLast(ll: LinkedList, k: Int): Node = {
    val counter = new Counter(0)
    findKthLast(ll.head, counter, k)
  }

  def findKthLast(iter: Node, counter: Counter, k: Int): Node = {
    if (iter == null) return null
    val node = findKthLast(iter.next, counter, k)
    counter.value = counter.value + 1
    if (counter.value == k) return iter
    return node
  }

  // Iterative algorithm: add runner1 at head and runner2 at a distance k
  // start iterating them and when runner2 = null, return runner1
  def deleteMiddle(n: Node): Boolean = {
    if (n == null || n.next == null) return false
    val next = n.next
    n.data = next.data
    n.next = next.next
    return true
  }

  // Given a LinkedList, delete a node in the middle
  // One runner traversing 1X, another runner traversing 2X
  // a -> b -> a -> c -> d
  // runner1 a, b, a
  // runner2 a, a, d
  // if (runner2.next == null) runner1->data is the middle node

  // Write code to partition a linked list around a value x
  case class Partition(before: LinkedList, after: LinkedList)
  def partition(ll: LinkedList, x: Int): Partition = {
    val before = new LinkedList
    val after = new LinkedList
    var iter = ll.head
    var tail: Node = null
    while(iter != null) {
      if (iter.data < x) tail = before.add(iter.data)
      else after.add(iter.data)
      iter = iter.next
    }
    Partition(before, after)
  }

  // Sum two list where each node has a single digit
  // 7 -> 1 -> 6 + 5 -> 9 -> 2 = 2 -> 1 -> 9
  def sum(l1: LinkedList, l2: LinkedList) : LinkedList = {
    var runner1 = l1.head
    var runner2 = l2.head
    var carry = 0
    val result = new LinkedList
    while (runner1 != null || runner2 != null) {
      val sum = runner1.data + runner2.data + carry // 8 + 6 = 14 % 10 = 4
      result.add(sum / 10)
      carry = sum % 10
      runner1 = runner1.next
      runner2 = runner2.next
    }
    result
  }

  // Intersection: Given two list find the reference of intersecting list
  // 3 -> 1 -> 5      4 -> 6
  //           -> 9 ->
  //      2 -> 7      8
  // 2 linked list can intersect and diverge
  // Put a pointer and start marking the reference
  // One list of length 6, another of 4 difference is 2
  // Longer list, traverse pointer by difference and then start 2 runners
  case class Tail(ll: Node, size: Int)

  def getTailAndSize(ll: LinkedList): Tail = {
    var size = 0
    var iter = ll.head
    while (iter.next != null) {
      iter = iter.next
      size += 1
    }
    Tail(iter, size)
  }

  def intersect(l1: LinkedList, l2: LinkedList): Node = {
    if (l1 == null || l2 == null) return null
    val result1 = getTailAndSize(l1)
    val result2 = getTailAndSize(l2)
    var shorter: Node = null
    var longer: Node = null

    if (result1.size < result2.size) {
      shorter = l1.head
      longer = l2.head
    } else {
      shorter = l2.head
      longer = l1.head
    }

    var i = Math.abs(result1.size - result2.size)
    while(i >= 0) {
      longer = longer.next
      i -= 1
    }

    while(shorter != longer) {
      shorter = shorter.next
      longer = longer.next
    }

    return longer
  }
}