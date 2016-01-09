package com.phmm.viterbi.process

import java.net.URL

import scala.io.Source
import scala.util.parsing.json._

/**
  * Machine of Profile Hidden Markov Model
  * @param transition Transition probability object
  * @param emission Emission probability object
  * @param matchStateNum Number of match state
  */
class Machine(
               transition: Map[String, Map[String, Double]],
               emission: Map[String, Map[String, Double]],
               matchStateNum: Int
             ) {

  /**
    * Run query for finding Viterbi path
    * @param s Viterbi path
    */
  def query(s: String): (String, Double) = {

    val viterbi = new Viterbi(transition, emission, matchStateNum)
    viterbi.run(s)
  }

}

object Machine {

  /**
    *
    * @param dataPath JSON data file path
    * @return Machine of profile hidden markov Model
    */
  def apply(dataPath: URL): Machine = {
    val data = JSON.parseFull(Source.fromURL(dataPath).mkString)

    val dataMap: Map[String, Any] = data match {
      case Some(x) => x.asInstanceOf[Map[String, Any]]
      case None => Map.empty
    }

    type mapType = Map[String, Map[String, Double]]

    val transition = dataMap.getOrElse("transition", Map.empty).asInstanceOf[mapType]
    val emission = dataMap.getOrElse("emission", Map.empty).asInstanceOf[mapType]
    val matchStateNum = dataMap.getOrElse("matchState", 0).asInstanceOf[Double].toInt

    new Machine(transition, emission, matchStateNum)
  }
}