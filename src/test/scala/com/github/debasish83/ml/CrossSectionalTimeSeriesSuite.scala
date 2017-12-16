package com.github.debasish83.ml

import com.cloudera.sparkts.models.Autoregression
import org.scalatest.{FunSuite, ShouldMatchers}
import java.io.PrintWriter
import java.io.File

import com.cloudera.sparkts.DateTimeIndex

class CrossSectionalTimeSeriesSuite extends FunSuite with ShouldMatchers {
  val path = "data/timeseries/data.csv"
  val ts = CrossSectionalTimeSeries.read(path)

  val outputPath = "data/timeseries/"

  test("read data and validate series") {
    ts.keys.length shouldBe 50
  }

  test("rmse without smoothing") {
    // TODO: Smoothing did not improve RMSE
    // CrossSectionalTimeSeries.smoothMovingAverage(ts, window = 7)

    val (rmse, output) = CrossSectionalTimeSeries.rmse(ts, maxLags = 7)
    val timestamps = output.index.toZonedDateTimeArray()
    val writer = new PrintWriter(new File("predictions.csv"))
    var row = 0
    while (row < output.data.numRows) {
      writer.print(s"${timestamps(row).toLocalDate}")
      var col = 0
      while (col < output.data.numCols) {
        writer.print(s",${output.data(row, col)}")
        col += 1
      }
      writer.println()
      row += 1
    }
    writer.close()
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
    val model = CrossSectionalTimeSeries.fit(ts, 7)
    assert(model.size == 50)
  }

  test("forecast for n days") {
    val timestamp = ts.index.toZonedDateTimeArray()

    val (testSamples, trainSamples) = timestamp.splitAt((timestamp.length * 0.1).toInt)

    val train = ts.mapSeries(v => v, DateTimeIndex.irregular(trainSamples))
    val test = ts.mapSeries(v => v, DateTimeIndex.irregular(testSamples))

    val maxLags = 7
    val models = CrossSectionalTimeSeries.fit(train, maxLags)
    CrossSectionalTimeSeries.forecast(test, models, maxLags, 31)
  }
}
