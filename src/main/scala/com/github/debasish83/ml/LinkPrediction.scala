package com.github.debasish83.ml

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.SparkSession

object LinkPrediction {
  def getFeatures(path: String): DataFrame = {
    val sc = SparkSession.builder().getOrCreate()
    sc.read.csv(path)
  }

  def getLabels(path: String): DataFrame = {
    ???
  }
}
