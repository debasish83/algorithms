package com.github.debasish83

import java.util.ArrayList

/**
  * @author debasish83 on 11/14/17.
  */
// Example stack with Int, can be templatized
class Node(data: Int) {
  val next: Node = _
}

object Node {
  def apply(data: Int): Node = new Node(data)
}

class Stack {
  private var top: Node = _

  def push(item: Int): Unit = {
    val t = new Node(item)
    t.next = top
    top = t
  }

  def pop(): Int = {
    if (top == null) throw new EmptyStackException
    val item = top.data
    top = top.next
    return item
  }

  def peek(): Int = {
    if (top == null) throw new EmptyStackException
    return top.data
  }

  def isEmpty(): Boolean = {
    return top == null
  }

  def removeBottom(): Int = {
    if (top.next == null) pop()

    var bottom = top
    // 2, 4, 3, 5 push(2), push(4), push(3) 3 -> 4 -> 2
    var iter = top.next

    while (iter.next != null) {
      bottom = iter
      iter = iter.next
    }
    bottom.next = null
    iter.data
  }
}

class FullStackException extends Exception
class EmptyStackException extends Exception

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

class MultiStack(stackSize: Int) {
  val numStacks = 3

  val stackCapacity = stackSize
  val values = Array.fill[Int](stackSize * numStacks)(0)
  val sizes = Array.fill[Int](numStacks)(0)

  def isFull(stackNum: Int): Boolean = {
    return sizes(stackNum) == stackCapacity
  }

  def isEmpty(stackNum: Int): Boolean = {
    return sizes(stackNum) == 0
  }

  def indexOfTop(stackNum: Int): Int = {
    val offset = stackNum * stackCapacity
    return offset + sizes(stackNum) - 1
  }

  def push(stackNum: Int, value: Int): Unit = {
    // Check if we have space for the next element
    if (isFull(stackNum)) throw new FullStackException
    sizes(stackNum) += 1
    values(indexOfTop(stackNum)) = value
  }

  def pop(stackNum: Int): Int = {
    if (isEmpty(stackNum)) throw new EmptyStackException
    val topIndex = indexOfTop(stackNum)
    val value = values(topIndex)
    values(topIndex) = 0
    sizes(stackNum) -= 1
    return value
  }

  def peek(stackNum: Int): Int = {
    if (isEmpty(stackNum)) throw new EmptyStackException
    return values(indexOfTop(stackNum))
  }
}

// Stack which has push/pop and a max function
// 2 4 3 2 5
// push(2) max = 2, index = 0
// push(4) if (max < 4) max = 4
// push(3) if (max < 3) max = 4
// pop(): 3 max = 4
// pop(): 4 max = 2
// We can add a stack with min values
class MinStack extends Stack {
  val mins: Stack = _

  override def push(item: Int): Unit = {
    if (item < min) mins.push(item)
    push(item)
  }

  def min(): Int = {
    if (mins.isEmpty()) return Integer.MAX_VALUE
    else mins.peek()
  }

  override def pop(): Int = {
    val item = pop()
    if (item == min()) mins.pop()
    item
  }
}

class SetOfStacks(n: Int) {
  val stacks: ArrayList[Stack] = new ArrayList[Stack](n)

  def push(v: Int): Unit = {
    ???
  }

  def getLastStack(): Stack = {
    if (stacks.isEmpty) return null
    return stacks.get(stacks.size() - 1)
  }

  def shift(index: Int, removeTop: Boolean): Int = {
    val stack = stacks.get(index)
    val removedItem = if (removeTop) stack.pop() else stack.removeBottom()
    if (stack.isEmpty()) {
      stacks.remove(index)
    } else if (stacks.size > index + 1) {
      val v = shift(index + 1, false)
      stack.push(v)
    }
    return removedItem
  }

  // If we want to popAt from a index, idea is to use a rollover system
  // Take element out of stack1 and then take bottom of stack2 and push to stack1
  // How do we define bottom of stock ?
  def popAt(index: Int): Int = shift(index, true)
}

// Sort a stack such that smallest items are on the top
// Stack1 and Stack2 Stack2 is in sorted order
object Stack {
  def sort(s: Stack): Unit = {
    val r = new Stack
    while (!s.isEmpty()) {
      val tmp = s.pop()
      while (!r.isEmpty() && r.peek() > tmp) s.push(r.pop)
      r.push(tmp)
    }

    //Copy the elements from r back to s
    while (!r.isEmpty()) s.push(r.pop)
  }
}
