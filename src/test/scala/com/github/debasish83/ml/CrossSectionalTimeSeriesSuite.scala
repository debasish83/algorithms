package com.github.debasish83.ml

import com.cloudera.sparkts.TimeSeries
import com.cloudera.sparkts.models.Autoregression
import org.scalatest.{FunSuite, ShouldMatchers}

class CrossSectionalTimeSeriesSuite extends FunSuite with ShouldMatchers {
  val path = "data/timeseries/data.csv"
  val ts = CrossSectionalTimeSeries.read(path)

  val outputPath = "data/timeseries/"

  test("read data and validate series") {
    ts.keys.length shouldBe 50
  }

  test("rmse with smoothing") {
    val rmse = CrossSectionalTimeSeries.rmse(ts, maxLags = 7, window = 7)
    println(s"rmse for 80-20 split ${rmse}")
  }

  test("smooth and fit per series AR model") {
    val maxLags = 7
    ts.univariateKeyAndSeriesIterator().foreach { case (key, v) =>
      if (key == "\"sig.0001\"") {
        val model = Autoregression.fitModel(v, maxLags, true)
        val modelCS = CrossSectionalTimeSeries.fitAR(ts, maxLags, key)
        assert(model.coefficients === modelCS)
      }
    }
  }

  test("read and fit cross section AR model") {
    val output = "data/timeseries/"
    val model = CrossSectionalTimeSeries.fitCrossSection(ts, 7)
    assert(model.size == 50)
  }
}
