package com.github.debasish83.ml

import org.scalatest.{FunSuite, ShouldMatchers}

class MarketplaceMatchSuite extends FunSuite with ShouldMatchers {
  val path = "data/matching/ds-rd1-sample.csv"
  val ds = MarketplaceMatch.read(path)
  ds.print()

  test("read csv") {
    assert(ds.features.size == 20)
  }
}
