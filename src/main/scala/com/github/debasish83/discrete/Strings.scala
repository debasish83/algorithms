package com.github.debasish83.discrete

import scala.collection.Map
import scala.collection.mutable.HashMap

/**
  * @author debasish83 Implement overridden versions of StringBuilder, HashTable, ArrayList
  * and ConcurrentHashMap
  */
object Strings {

  // Algorithm to figure out if a string has all unique characters
  def isUnique(str: String): Boolean = {
    val numChars = 256
    val uniqueChars = new Array[Boolean](numChars)
    var i = 0
    while (i < str.length) {
      val c = str.charAt(i)
      if (uniqueChars(c)) return false
      else uniqueChars(c) = true
      i += 1
    }
    true
  }

  // Additional question: we can't use a data-structure
  // Here the idea will be to pick a character and then run a binary search
  // over rest of the characters to see if there is hit
  // Memoize the character that was put

  // Given two strings,write a method to decide if one is a permutation of the other.
  // abcd and bcad are permutations of each other ?
  def isPermutation(stra: String, strb: String): Boolean = {
    val charMap = HashMap.empty[Char, Int]
    var i = 0
    while (i < stra.length) {
      val c = stra.charAt(i)
      if (charMap.contains(c)) charMap(c) += 1
      else charMap(c) = 0
    }
    i = 0
    while (i < strb.length) {
      val c = stra.charAt(i)
      if (charMap.contains(c)) charMap(c) -= 1
      else return false
    }
    val iter = charMap.iterator
    while (iter.hasNext) {
      if (iter.next()._2 != 0) return false
    }
    return true
  }

  // Palindrome permutation: given a string, write a function to check if it is permutation
  // of a palindrome, for palindrome forward/backward are same
  // Tact Coa permutations: "taco cat", "atco cta" 2t 2a 2c 1o
  // Tact Coa build a table of frequency
  def buildFrequencyTable(input: String): Map[Char, Int] = {
    val charMap = HashMap.empty[Char, Int]

    var i = 0
    while (i < input.length) {
      val c = input.charAt(i)
      if (charMap.contains(c)) charMap(c) += 1
      else charMap(c) = 1
      i += 1
    }
    charMap
  }

  def checkOdd(table: Map[Char, Int]): Boolean = {
    var foundOdd = false
    val iter = table.iterator
    while (iter.hasNext) {
      if (iter.next()._2 % 2 == 1) {
        // If the frequency is odd
        if (foundOdd) return false
        foundOdd = true
      }
    }
    return true
  }

  def palindromePermute(input: String): Boolean = {
    val table = buildFrequencyTable(input)
    return checkOdd(table)
  }

  import scala.collection.mutable.Map

  // out = ("hello" -> 1, "world" -> 2)

  // step1: Construct the regex [,!]+ regex = "[,! ]+"
  // Blacklists Array(',', '!',' ')
  // Whitelist: [a - z, A - Z]
  import scala.util.matching.Regex

  def uniqueWhitelist(str: String,
                      whitelist: String = "[a-zA-Z]+"): Map[String, Int] = {
    val r: Regex = whitelist.r
    val map = Map.empty[String, Int]

    r.findAllIn(str).foreach(token => {
      val key = token.toLowerCase
      if (map.contains(key)) map.put(key, map.get(key).get + 1)
      else map.put(key, 1)
    })
    return map
  }

  //step2: build map with token.toLowerCase
  def unique(str: String, delimiter: String = " "): Map[String, Int] = {
    val regex = s"$delimiter+"

    // https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
    val tokens = str.split(regex)
    //TODO: Understand HashTable usage
    val map = new HashMap[String, Int]()

    tokens.foreach(token => {
      if (map.contains(token)) {
        val cnt = map.get(token).get + 1
        map.put(token, cnt)
      }
      else map.put(token, 1)
    })
    return map
  }
}
