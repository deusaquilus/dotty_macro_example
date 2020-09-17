package org.stuff

import Dsl._

object UseSimpleQuill2 {
  case class Person(name:String)

  def main(args: Array[String]):Unit = {
    //inline def q = quote { query[Person] }
    //PrintMac(q)
    //println(q)
    PrintMac { 4: Long }
  }
}