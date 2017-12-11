package com.github.debasish83.discrete

// Queue adds a node to last and removes from first
class Queue {
  private var first: Node = _
  private var last: Node = _

  def add(item: Int): Unit = {
    val t = new Node(item)
    if (last != null) last.next = t
    last = t
    if (first == null) first = last
  }

  def remove(): Int = {
    if (first == null) throw new NoSuchElementException
    val data = first.data
    first = first.next
    if (first == null) last = null
    data
  }

  def peek(): Int = {
    if (first == null) throw new NoSuchElementException
    return first.data
  }

  def isEmpty(): Boolean = {
    return first == null
  }
}

import scala.collection.mutable.{Stack => SStack}
import scala.collection.mutable.{Queue => SQueue}

// In a queue it's first in, first out
//
class QueueUsingStack {
}
//Implement a queue using 2 stacks
object Queue {
  def find(elem: Int, queue: SQueue[Int]): Boolean = {
    //queue.enqueue(elem)
    //queue.dequeue()
    val iter = queue.iterator
    while (iter.hasNext) {
      if (iter.next() == elem) return true
    }
    return false
  }

  //There is no guarantee that queue is sorted
  def findBinary(elem: Int, queue: SQueue[Int]): Boolean = {
    //We do a linear scan and add elements of the queue in sorted order by
    //keeping track of max/min
    //Find which position to insert is also O(logn)
    ???
  }
}