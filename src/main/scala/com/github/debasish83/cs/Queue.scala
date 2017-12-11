package com.github.debasish83.cs

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
