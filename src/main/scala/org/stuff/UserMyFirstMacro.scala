package org.stuff

object UserMyFirstMacro {
  // def one():Unit = {
  //    val output = MyFirstMacro.myMac("hello") //hello
  //    println(output)

  //    val v = "hello"
  //    val output1 = MyFirstMacro.myMac(v) //hello
  //    println(output1)

  //    inline def vv = "hello"
  //    val output2 = MyFirstMacro.myMac(vv) //hello
  //    println(output2)

  //    inline def vvv = "hello"
  //    val output3 = MyFirstMacro.myMac2(vvv) //hellooooooooo
  //    println(output3)
  // }

  def two():Unit = {
    val somethingElse = "onetwothree"

     inline def second = "blahblah"
     inline def vv = second + somethingElse
     inline def output2 = MyFirstMacro.myMac(vv) //hellooooooooooooo
     inline def vvv = output2

     //inline def vvvv = "somethingelse"
     val output3 = MyFirstMacro.myMac2(vvv) //hellooooooooooooooooooooooooo
     println(output3)
  }

  def main(args: Array[String]):Unit = {
    two()
  }
}
