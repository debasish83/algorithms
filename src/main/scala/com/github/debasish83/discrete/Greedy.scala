package com.github.debasish83.discrete

import scala.math.Ordering

object Greedy {
  // fractional knapsack: there are values and weights, generate ratio of value/weight do a sort and then pick
  // as much as you can till W is met.
  def fractionalKnapsack(W: Int, values: Array[Int], weights: Array[Int]): Int = {
    val ratio = Array.ofDim[Double](values.length)
    for (i <- 0 until values.length) ratio(i) = values(i) / weights(i)
    ratio.zipWithIndex.sorted(Ordering[Double].reverse)
    // Pick up the weights till we reach W and calculate the cost
    ???
  }

  //Huffman encoding
}
