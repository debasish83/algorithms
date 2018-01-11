package com.github.debasish83.ml

import org.apache.spark.sql.{DataFrame, SparkSession}
import scala.collection.mutable.ArrayBuffer

/*
 Service providers need to be matched with Customer to complete their request
 1. Customer creates a request and request is matched with hundreds of service providers
 2. The top5 service provider that's similar to the request are selected and invites are sent
 3. If service provider is interested, he/she will quote on the request
 4. A maximum of 5 quotes is allowed for each request
 */

import scala.io.Source
import util.control.Breaks._

object MarketplaceMatch {
  def read(sess: SparkSession, path: String): DataFrame = {
    sess.read.csv(path)
  }

  case class Feature(values: Array[String]) {
    override def toString(): String = values.mkString(",")
  }

  case class Dataset(features: Array[Feature]) {
    def print(): Unit = {
      features.foreach(println(_))
    }
  }

  /* Example data with header
    invite_id	request_id	user_id.customer	user_id.provider	category_id	location_id	location	category	creation_time	sent_time	quote
    13912	2764	3764	764	1	4	Dallas-Fort Worth-Arlington, TX	Photography	8/4/2013 10:39:54	8/4/2013 18:21:52	0
  */
  def read(path: String): Dataset = {
    val bufferedSource = Source.fromFile(path)
    val iter = bufferedSource.getLines()

    val buffer = new ArrayBuffer[Feature]

    var lines = 0
    while (iter.hasNext) {
      val line = iter.next()
      //skip header
      breakable {
        if (lines == 0) break()
        val cols = line.split(",").map(_.trim)
        buffer += Feature(cols)
      }
      lines += 1
    }
    println(s"csv lines ${lines}")
    Dataset(buffer.toArray)
  }
}
