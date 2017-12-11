package com.github.debasish83.ml

import java.util.Arrays
import com.github.fommil.netlib.BLAS.{getInstance => blas}

/** Representing a normal equation. */
class NormalEquation(val k: Int) extends Serializable {
  /** Number of entries in the upper triangular part of a k-by-k matrix. */
  val triK = k * (k + 1) / 2
  /** A^T^ * A */
  val ata = new Array[Double](triK)
  /** A^T^ * b */
  val atb = new Array[Double](k)
  /** Number of observations. */
  var n = 0

  private val da = new Array[Double](k)
  private val upper = "U"

  /** Adds an observation. */
  def add(a: Array[Double], b: Double): this.type = {
    require(a.length == k)
    blas.dspr(upper, k, 1.0, da, 1, ata)
    blas.daxpy(k, b.toDouble, da, 1, atb, 1)
    n += 1
    this
  }

  /** Resets everything to zero, which should be called after each solve. */
  def reset(): Unit = {
    Arrays.fill(ata, 0.0)
    Arrays.fill(atb, 0.0)
    n = 0
  }
}
