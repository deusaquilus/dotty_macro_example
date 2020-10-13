package org.stuff

import Dsl._

object UseSimpleQuill4 {

  def main(args: Array[String]):Unit = { //hello
    val a:Int = 1
    val b:Int = 2
    // inline def q = a == b

    ParseMac(a == b) //hellooo

    //PrintMac(q1)
    
    //println(pprint.apply(q))
  }
}