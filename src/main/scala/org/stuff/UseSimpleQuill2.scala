package org.stuff

import Dsl._

object UseSimpleQuill2 {

  case class Person(name:Boolean)

  def main(args: Array[String]):Unit = {
    inline def q = quote { query[Person].filter(p => p.name) } // Quoted[Query[Person]](Entity("Person"))
    PrintMac(q)
    println(q)
  }
}