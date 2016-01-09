package com.phmm.viterbi.process

import com.phmm.viterbi.commons.Constant

import scala.collection.mutable.{Map => MutableMap}

/**
  * Modified Viterbi algorithm for bioinformatics
  * @param transition Transition probability object
  * @param emission Emission probability object
  * @param matchStateNum Number of match state
  */
class Viterbi(
               transition: Map[String, Map[String, Double]],
               emission: Map[String, Map[String, Double]],
               matchStateNum: Int
             ) {

  /**
    * Viterbi algorithm
    * @param queryString Query string for viterbi algorithm
    * @return Viterbi path
    */
  def run(queryString: String): (String, Double) = {
    val query = "." + queryString
    val matrix = initialize(query.length, matchStateNum)
    val roadMap = MutableMap[Int, MutableMap[String, String]]()

    val maxForTuple = (a: (Double, String), b: (Double, String)) => if (a._1 > b._1) a else b
    val finalMatch = getStates(matchStateNum)._1

    for (i <- matrix.indices;
         j <- matrix(i).indices;
         (currentMatch, currentInsert, currentDelete) = getStates(j);
         (prevMatch, prevInsert, prevDelete) = getStates(j - 1);
         k <- matrix(i)(j).indices
         if !((j == 0 && (k == 0 || k == 2)) || (i == 0 && (j == 0 || k == 0 || k == 1)))) {

      matrix(i)(j)(k) = k match {
        case 0 => // for match state
          val (maxValue, from) = Array[Double](
            matrix(i - 1)(j - 1)(0) * getTransitionProb(prevMatch, currentMatch, 0.0),
            matrix(i - 1)(j - 1)(1) * getTransitionProb(prevInsert, currentMatch, 0.0),
            if (j != 0) matrix(i - 1)(j - 1)(2) * getTransitionProb(prevDelete, currentMatch, 0.0) else -1
          ).zip(Array(prevMatch, prevInsert, prevDelete)).reduceLeft(maxForTuple)
          if (maxValue != 0) roadMap.getOrElseUpdate(i, MutableMap()) += (currentMatch -> from)
          getEmissionProb(currentMatch, query(i).toString, 0.0) * maxValue
        case 1 => // for insert state
          val (maxValue, from) = Array[Double](
            matrix(i - 1)(j)(0) * getTransitionProb(currentMatch, currentInsert, 0.0),
            matrix(i - 1)(j)(1) * getTransitionProb(currentInsert, currentInsert, 0.0),
            if (j != 0) matrix(i - 1)(j)(2) * getTransitionProb(currentDelete, currentInsert, 0.0) else -1
          ).zip(Array(currentMatch, currentInsert, currentDelete)).reduceLeft(maxForTuple)
          if (maxValue != 0) roadMap.getOrElseUpdate(i, MutableMap()) += (currentInsert -> from)
          0.25 * maxValue
        case 2 if j != 0 => // for delete state
          val (maxValue, from) = Array[Double](
            matrix(i)(j - 1)(0) * getTransitionProb(prevMatch, currentDelete, 0.0),
            matrix(i)(j - 1)(1) * getTransitionProb(prevInsert, currentDelete, 0.0),
            if (j != 1) matrix(i)(j - 1)(2) * getTransitionProb(prevDelete, currentDelete, 0.0) else -1
          ).zip(Array(prevMatch, prevInsert, prevDelete)).reduceLeft(maxForTuple)
          if (maxValue != 0) roadMap.getOrElseUpdate(i, MutableMap()) += (currentDelete -> from)
          maxValue
        case 3 => // This works for only final state
          val (maxValue, from) = Array[Double](
            matrix(i)(j)(0) * getTransitionProb(currentMatch, finalMatch, 0.0),
            matrix(i)(j)(1) * getTransitionProb(currentInsert, finalMatch, 0.0),
            matrix(i)(j)(2) * getTransitionProb(currentDelete, finalMatch, 0.0)
          ).zip(Array(currentMatch, currentInsert, currentDelete)).reduceLeft(maxForTuple)
          if (maxValue != 0) roadMap.getOrElseUpdate(i, MutableMap()) += (finalMatch -> from)
          maxValue
      }
    }
    /*
    for (i <- matrix.indices) {
      println(s"char $i")
      for (j <- matrix(i).indices) {
        println(s"state $j")
        println(matrix(i)(j).mkString("    "))
      }
    }
    */
    (traceback(finalMatch, queryString.length, roadMap), matrix.last.last.last)
  }

  /**
    * Traceback algorithm for Viterbi path
    * @param finalState End of the match state
    * @param queryLen Length of query string
    * @param roadMap Data for traceback algorithm
    * @return Viterbi path
    */
  private def traceback(finalState: String,
                        queryLen: Int,
                        roadMap: MutableMap[Int, MutableMap[String, String]]): String = {
    var result = finalState
    var traceback = roadMap.get(matchStateNum - 1).get(finalState)
    var charNum = queryLen
    while (traceback != "M0") {
      traceback = roadMap.get(charNum).get(traceback)
      charNum = if (traceback(0) == 'M' || traceback(0) == 'I') charNum - 1 else charNum
      result = traceback + "-" + result
    }
    result
  }

  /**
    *
    * @param queryLen Length of query string for dynamic programming approach
    * @param matchStateNum Number of match state
    * @return three dimensional matrix
    */
  private def initialize(queryLen: Int, matchStateNum: Int): Array[Array[Array[Double]]] = {
    val matrix = Array.ofDim[Double](queryLen, matchStateNum, Constant.STATE_NUM)
    matrix(queryLen - 1)(matchStateNum - 1) = new Array[Double](Constant.STATE_NUM + 1)
    matrix(0)(0)(0) = 1
    matrix
  }

  /**
    *
    * @param from Prev State
    * @param to Next State
    * @param default Default transition probability
    * @return transition probability
    */
  private def getTransitionProb(from: String, to: String, default: Double = 0.0): Double = {
    transition.getOrElse(from, Map.empty).getOrElse(to, default)
  }

  /**
    *
    * @param where State
    * @param what Char
    * @param default Default emission probability
    * @return emission probability
    */
  private def getEmissionProb(where: String, what: String, default: Double): Double = {
    emission.getOrElse(where, Map.empty).getOrElse(what, default)
  }

  /**
    *
    * @param num Current state number
    * @return Match, Insert and Delete state with number
    */
  private def getStates(num: Int): (String, String, String) = {
    (s"M$num", s"I$num", if (num != 0) s"D$num" else "")
  }
}
