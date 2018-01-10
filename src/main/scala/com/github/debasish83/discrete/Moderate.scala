package com.github.debasish83.discrete

import java.util.Comparator

case class Person(birthYear: Int, deathYear: Int)

//Sort options
//Use scala.util.Sorting for quicksort
//Array[Person].sorted which internally calls java.util.Arrays.sort
//java.util.Arrays.sort Uses legacy merge sort OR TimSort
//scala.util.Sorting implements quicksort

object Moderate {

  // 1800 1870
  // 1870 1870
  // 1870 1950
  class BirthOrder extends Ordering[Person] {
    def compare(x: Person, y: Person): Int = {
      x.birthYear - y.birthYear
    }
  }

  class DeathOrder extends Ordering[Person] {
    def compare(x: Person, y: Person): Int = {
      x.deathYear - y.deathYear
    }
  }

  import java.util.ArrayList
  import scala.collection.mutable.Map
  import java.util.Collections
  import java.util.Arrays

  //Keep an list of keys and respective count in as hashMap, run a binary search
  //in the list of keys to find insertion point and update the keys after that
  //binary search is fine but key update can be a O(n) operation !

  //1800 1870
  //1850 1900
  //1880 1950

  //step1: iterate over birth Year
  //Find 1800 position using binary search and increment count till we reach 1870

  //1850, 1900
  //Find 1850 position using binary search and then increment count till we hit 1900
  class IntCmp extends Comparator[Int] {
    def compare(o1: Int, o2: Int) = o1 - o2
  }

  // If it is Person we can extends Comparator
  def mostPeopleAliveInterval(census: Array[Person]): Int = {
    val years = new ArrayList[Int]()
    val aliveYear = Map.empty[Int, Int]

    val cmp = new IntCmp
    val ord = new BirthOrder

    val birthSorted = census.sorted(ord)
    var i = 0
    while (i < birthSorted.length) {
      //find the index of exact match or index of insert
      //Arrays.binarySearch gives an insertion point if it does not exist
      val index = Collections.binarySearch(years, birthSorted(i).birthYear, cmp)
      var iter = if (index < 0) {
        // if key does not exist -(<i>insertion point</i>) - 1)
        -index - 1
      } else {
        index
      }

      while (iter < years.size) {
        if (years.get(iter) <= birthSorted(i).deathYear) {
          aliveYear(years.get(iter)) += 1
        }
        iter += 1
      }

      // insert birthYear at index so that years stay sorted
      if (index < 0) {
        years.add(index, birthSorted(i).birthYear)
        aliveYear.put(birthSorted(i).birthYear, 1)
      }

      // insert deathYear at end of list so that years stay sorted
      if (!aliveYear.contains(birthSorted(i).deathYear)) {
        years.add(birthSorted(i).deathYear)
        aliveYear.put(birthSorted(i).birthYear, 1)
      }
      i += 1
    }
    //aliveYear has year, peopleAlive, we have to sort on peopleAlive and pick up year
    var maxAlive = 0
    var maxYear = 0
    i = 0
    while (i < years.size) {
      val currAlive = aliveYear(i)
      if (currAlive > maxAlive) {
        maxAlive = currAlive
        maxYear = i
      }
      i += 1
    }
    return maxAlive
  }

  def mostPeopleAlive(census: Array[Person]): Int = {
    // sort on birthYear
    // sort on deathYear

    // increment if a birthYear has changed, decrement if we hit a death
    // update the maxAliveYear through some logic

    // Sorting.quickSort(census)

    val minYear = census.map(_.birthYear).min
    val maxYear = census.map(_.deathYear).max

    val iord = Ordering[Int]
    val birthSorted = census.map(_.birthYear).sorted(iord)
    val deathSorted = census.map(_.deathYear).sorted(iord)

    var birthIndex = 0
    var deathIndex = 0

    var maxAlive = 0
    var currentAlive = 0
    var maxAliveYear = minYear

    while (birthIndex < birthSorted.length) {
      if (birthSorted(birthIndex) <= deathSorted(deathIndex)) {
        currentAlive += 1
        if (currentAlive > maxAlive) {
          maxAlive = currentAlive
          maxAliveYear = birthSorted(birthIndex)
        }
        birthIndex += 1
      } else if (birthSorted(birthIndex) > deathSorted(deathIndex)) {
        currentAlive -= 1
        deathIndex += 1
      }
    }
    return maxAliveYear
  }

  // Number swapper: Function to swap a number in place
}
