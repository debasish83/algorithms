package com.github.debasish83.discrete

import java.util.ArrayList

/**
  * @author debasish83 on 11/14/17.
  */

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
  val mins: Stack = null

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

  //Given an expression figure out if it has balanced braces
  //Array('{', '}', ']', '[') : False
  //Array('{', '[', '[', '(', ')', ']', ']', '}', '{', '}') : True

  import java.util.{Stack => JStack}

  def balancedBraces(elems: Array[Char]): Boolean = {
    val balancedSoFar = new JStack[Char]()

    var i = 0
    while (i < elems.length) {
      if (elems(i) == '{' || elems(i) == '[' || elems(i) == '(')
        balancedSoFar.push(elems(i))
      else {
        if (elems(i) == '}' && balancedSoFar.peek == '{') balancedSoFar.pop()
        else if (elems(i) == ']' && balancedSoFar.peek == '[') balancedSoFar.pop()
        else if (elems(i) == ')' && balancedSoFar.peek == '(') balancedSoFar.pop()
        else return false
      }
      i += 1
    }
    if (balancedSoFar.size > 0) return false
    else return true
  }

  // "{abc }abc {ab } = compiles
  // "{a[b]c } => compiles
  // "{ a(b)c } => compiles
  // Stack { [
  // ] } compiles
  // } ] !compiles

  import java.util.Stack

  def isCompiles(str: String): String = {
    val stack = new JStack[Char]()
    var i = 0
    while(i < str.length) {
      val char = str(i)

      if (char == '{' || char == '[') stack.push(char)

      if (char == '}') {
        if (stack.size > 0) {
          val head = stack.peek()
          if (head == '{') stack.pop()
          else return "!compiles"
        }
        else return "!compiles"
      }

      if (char == ']') {
        if (stack.size > 0) {
          val head = stack.peek()
          if (head == '[') stack.pop()
          else return "!compiles"
        }
        else return "!compiles"
      }
      i += 1
    }

    if (stack.isEmpty) return "compiles"
    else "!compiles"
  }
}


/*
Write a method to tell me if this compiles:
"{ somehoaiwjfsdkf }" -> compiles
"{[]}" -> compiles
"{[}]" -> !compiles
"{" -> !compiles
“{[][]}” -> compiles
"{[{}{}][]}" -> compiles

"<tag></tag>" -> compiles
"<tag>" -> !compile

"openings = List({, [, (, <tag>, <html>, <b>)
"closings = List(}, ], ), </tag>)

// str = "{abc{<tag></tag>}}"
// return:
List ( ("{", true),("{", true), ("<tag>", true ), ("</tag">, false) ...)
parser(str: String): List[(String, Boolean)]

def isCompiles(str: String) : Boolean = {
  val patterns = parser(str)
  val stack = new Stack[Char]()
  var i = 0
  while(i < patterns.length) {
    val pattern = patterns(i)
    if(pattern._2) stack.push(pattern._1) Stack({, {, "<tag>")
    else {
      //pattern = '</tag>'
      val head = stack.peek() // </tag>
      pattern._1 match {
      case "{" => if (head != '}') return "!compiles" else stack.pop
      case "[" => if (head != ']') return "!compiles" else stack.pop
      case "</tag"> => if (head != '<tag>') return "!compiles else stack.pop
    }
    i += 1
  }

  if (stack.empty) return "compiles"
  else return "!compiles"
}
*/