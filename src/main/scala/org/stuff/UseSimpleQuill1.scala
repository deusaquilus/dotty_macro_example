package org.stuff


case class Person(name:String)

object UseSimpleQuill1 {
  def main(args: Array[String]):Unit = {
    // // '{ yay.toUpperCase }
    // inline def ret = SimpleQuill1.getMyTree("hello how are you".toUpperCase) //hellooo
    // val ret1 = SimpleQuill1.printTheTree(ret)
    // println(ret)
    

    import SimpleQuill1._
    inline def initial = query[Person]
    val ret0 = SimpleQuill1.printTheTree(initial)
    inline def myFirstQuery = quote(query[Person])
    val ret1 = SimpleQuill1.printTheTree(myFirstQuery)
  }  
}

