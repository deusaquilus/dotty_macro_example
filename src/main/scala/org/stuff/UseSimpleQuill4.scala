package org.stuff

import io.getquill._
import Dsl._

object UseSimpleQuill4 {

  case class Person(name:String, age:Int)

  def main(args: Array[String]):Unit = { //hello
     val foo: Query[Person] = null
     val bar: Query[Person] = null
     inline def myUnion = foo.union(bar)
     //PrintMac(myUnion)
     //println(myUnion) //hellooo
     ParseMac(myUnion)

     //inline def myString = "hello"
     //PrintMac((("hello": String): String): String) //helloooooooooooooooooooo
  }
}