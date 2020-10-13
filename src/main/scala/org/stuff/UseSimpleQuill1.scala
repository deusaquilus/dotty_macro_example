package org.stuff

import io.getquill.ast._

object UseSimpleQuill1 {
  case class Person(firstName:String, lastName:String, age:Int)
  implicit class PersonExt(person: Person) {
    def title = person.firstName + " " + person.lastName
  }

  implicit class NameExt[A](a:A) {
    def ->>[B](b:B) = a.toString + " " + b.toString
  }

  def main(args: Array[String]):Unit = {
    val p = Person("Joe", "Bloggs", 123)

    //inline def q = new PersonExt(p).title
    inline def q = p.->>("bar")
    PrintMac(q)

    println(q)

    // // '{ yay.toUpperCase }
    // inline def ret = SimpleQuill1.getMyTree("hello how are you".toUpperCase) //hellooo
    // val ret1 = SimpleQuill1.printTheTree(ret)
    // println(ret)
    

    // import SimpleQuill1._
    // inline def initial = query[Person]
    // val ret0 = SimpleQuill1.printTheTree(initial)
    // inline def myFirstQuery = quote(query[Person])
    // val ret1 = SimpleQuill1.printTheTree(myFirstQuery)

  }  
}

