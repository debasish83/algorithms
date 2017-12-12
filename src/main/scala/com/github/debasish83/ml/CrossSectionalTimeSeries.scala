package com.github.debasish83.ml

import com.cloudera.sparkts.{DateTimeIndex, EasyPlot, TimeSeries}
import com.cloudera.sparkts.parsers.YahooParser
import com.cloudera.sparkts.models.{ARModel, EWMA}
import org.apache.spark.mllib.linalg.DenseVector
import scala.io.Source
import java.util.ArrayDeque
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import org.apache.spark.mllib.linalg.DenseMatrix

object CrossSectionalTimeSeries {
  def read(path: String): TimeSeries[String] = {
    val text = Source.fromFile(path).mkString
    val dataset = YahooParser.yahooStringToTimeSeries(text)
    dataset
  }

  def plot(outputPath: String, ts: TimeSeries[String]): Unit = {
    val iter = ts.univariateKeyAndSeriesIterator()
    while (iter.hasNext) {
      val ts = iter.next()
      val f = EasyPlot.ezplot(ts._2)
      f.saveas(s"$outputPath${ts._1}.png")
    }
  }

  def smoothEWMA(outputPath: String, ts: TimeSeries[String]) = {
    val iter = ts.univariateKeyAndSeriesIterator()
    while (iter.hasNext) {
      val ts = iter.next()
      val model = EWMA.fitModel(ts._2)
      val smoothed = new DenseVector(Array.fill(ts._2.size)(0.0))
      model.addTimeDependentEffects(ts._2, smoothed)
      val f = EasyPlot.ezplot(smoothed)
      f.saveas(s"$outputPath${ts._1}_smooth.png")
    }
  }

  def plot(outputPath: String,
           prefix: String,
           ts: TimeSeries[String]): Unit = {
    val iter = ts.univariateKeyAndSeriesIterator()
    while (iter.hasNext) {
      val (key, univts) = iter.next()
      val f = EasyPlot.ezplot(univts)
      f.saveas(s"${outputPath}${key}_${prefix}.png")
    }
  }

  // Smooth the time series in-place
  def smoothMovingAverage(ts: TimeSeries[String],
                          window: Int) : Unit = {
    val queue = new ArrayDeque[Double](window)
    val iter = ts.univariateKeyAndSeriesIterator()
    while (iter.hasNext) {
      val univariatets = iter.next()._2.asInstanceOf[DenseVector]
      var i = 0
      while (i < univariatets.size) {
        if (queue.size() >= window) queue.poll()
        queue.push(univariatets(i))
        var cumsum = 0.0
        val iter = queue.iterator()
        while (iter.hasNext) {
          cumsum += iter.next()
        }
        require(queue.size <= window, s"${queue.size} $window")
        univariatets.values(i) = cumsum / queue.size()
        i += 1
      }
    }
  }
  // Lag the smoothed time series with AR(1)

  // Algorithm:
  // 1. Generate one model for each timeseries and see performance on test/validation data using OLS
  // 2. Generate 30 models and see the improvement on test/validation data using OLS
  def laggedCSKey(key: String, lagOrder: Int): String = {
    if (lagOrder > 0) s"lag:${key}:${lagOrder}" else key
  }

  def rmse(ts: TimeSeries[String], maxLags: Int): (Double, TimeSeries[String]) = {
    val timestamp = ts.index.toZonedDateTimeArray()

    val (testSamples, trainSamples) = timestamp.splitAt((timestamp.length * 0.2).toInt)

    val train = ts.mapSeries(v => v, DateTimeIndex.irregular(trainSamples))
    val test = ts.mapSeries(v => v, DateTimeIndex.irregular(testSamples))

    val models = fit(train, maxLags)

    val output =
      new DenseMatrix(test.data.numRows, test.data.numCols,
        Array.fill[Double](test.data.values.length)(0.0))
    val rmse = predict(test, maxLags, models, output)
    val outputts = new TimeSeries[String](test.index, output, test.keys)
    (rmse, outputts)
  }

  def predict(ts: TimeSeries[String], maxLags: Int, models: Map[String, ARModel],
              output: DenseMatrix): Double = {
    val laggedts = ts.lags(maxLags, true, laggedCSKey)

    var sqerr = 0.0
    var i = 0
    while(i < ts.keys.length) {
      val key = ts.keys(i)
      val model = models(ts.keys(i))
      val predicted = predict(laggedts, key, i, model, maxLags, ts.keys.length, output)
      sqerr += predicted
      i += 1
    }

    val instruments = ts.keys.length
    val rows = ts.data.numRows
    println(s"sqerr ${sqerr} instruments ${instruments} rows ${rows}")
    Math.sqrt(sqerr / ts.keys.length / ts.data.numRows)
  }

  def predict(laggedts: TimeSeries[String],
              key: String,
              i: Int,
              model: ARModel,
              maxLags: Int,
              len: Int,
              output: DenseMatrix): Double = {
    val numRows = laggedts.data.numRows
    val numCols = laggedts.data.numCols
    val laggedKeys = laggedts.keys

    val features = Array.fill[Double](maxLags * len)(0.0)
    var label = 0.0
    var err = 0.0

    var row = 0
    while (row < numRows) {
      var j = 0
      var col = 0
      while (col < numCols) {
        if (key == laggedKeys(col)) label = laggedts.data(row, col)
        else if (laggedKeys(col).contains(s"lag")) {
          features(j) = laggedts.data(row, col)
          j += 1
        }
        col += 1
      }

      val predicted = blas.ddot(features.length, features, 1, model.coefficients, 1)
      output.values(row * output.numCols + i) = predicted
      err += Math.pow(predicted - label, 2)
      row += 1
    }
    err
  }

  def fit(ts: TimeSeries[String], maxLags: Int): Map[String, ARModel] = {
    //sig1, sig1_lag1, sig1_lag2, sig2, sig2_lag2, ...
    val laggedts = ts.lags(maxLags, true, laggedCSKey)
    ts.keys.map((key) => {
      val model = fit(laggedts, maxLags, key, ts.keys.length)
      (key, model)
    }).toMap
  }

  def fit(laggedts: TimeSeries[String],
          maxLags: Int,
          key: String,
          len: Int): ARModel = {
    //sig1, sig1_lag1, sig1_lag2, sig2, sig2_lag2, ...
    val laggedKeys = laggedts.keys

    val features = Array.fill[Double](maxLags * len)(0.0)
    var label = 0.0

    val numRows = laggedts.data.numRows
    val numCols = laggedts.data.numCols

    val Y = Array.fill[Double](numRows)(0.0)
    val X = Array.fill[Array[Double]](numRows)(null)

    var row = 0
    while (row < numRows) {
      var j = 0
      var col = 0
      while (col < numCols) {
        if (key == laggedKeys(col)) label = laggedts.data(row, col)
        else if (laggedKeys(col).contains(s"lag")) {
          features(j) = laggedts.data(row, col)
          j += 1
        }
        col += 1
      }
      Y(row) = label
      X(row) = features.clone()
      row += 1
    }

    val regression = new OLSMultipleLinearRegression()
    regression.setNoIntercept(true)

    regression.newSampleData(Y, X)
    val params = regression.estimateRegressionParameters()
    new ARModel(0.0, params)
  }

  def fitAR(ts: TimeSeries[String], maxLags: Int, model: String): Array[Double] = {
    val key = ts.keys.filter { case (p) => p == model }(0)

    //sig1, sig1_lag1, sig1_lag2, sig2, sig2_lag2, ...
    val laggedts = ts.lags(maxLags, true, laggedCSKey)
    val laggedKeys = laggedts.keys

    val features = Array.fill[Double](maxLags)(0.0)
    var label = 0.0

    val numRows = laggedts.data.numRows

    val Y = Array.fill[Double](numRows)(0.0)
    val X = Array.fill[Array[Double]](numRows)(null)

    var col = 0
    while (key == laggedKeys(col)) {
      col += 1
    }

    var row = 0
    while (row < numRows) {
      label = laggedts.data(row, col - 1)
      var j = 0
      while (j < maxLags) {
        features(j) = laggedts.data(row, col + j)
        j += 1
      }
      Y(row) = label
      X(row) = features.clone()
      row += 1
    }

    val regression = new OLSMultipleLinearRegression()
    regression.setNoIntercept(true)

    regression.newSampleData(Y, X)
    val params = regression.estimateRegressionParameters()
    params
  }
}
