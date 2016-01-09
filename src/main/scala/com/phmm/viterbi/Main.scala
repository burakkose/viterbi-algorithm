package com.phmm.viterbi

import com.phmm.viterbi.process.Machine

object Main extends App {
  val test1 = getClass.getResource("/test1.txt")
  val test2 = getClass.getResource("/test2.txt")
  val test3 = getClass.getResource("/test3.txt")

  val machine1 = Machine(test1)
  val machine2 = Machine(test2)
  val machine3 = Machine(test3)

  /*
    // FIX HERE

  println(machine1.query("TAG"))
  println(machine2.query("TAG"))
  println(machine3.query("TAG"))
  */
}
