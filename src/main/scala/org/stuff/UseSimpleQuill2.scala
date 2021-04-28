package org.stuff

object UseSimpleQuill2 {
  import MiniQuill._
  import MiniQuill.Dsl._

  case class Person(isSober:Boolean, isRussian:Boolean, isHuman:Boolean)

  def main(args: Array[String]):Unit = {
    inline def q: Quoted[Query[Person]] = quote { 
      query[Person].filter(p => p.isSober) //  Query[Person] //hello
    }

    //inline def q1 = quote(unquote(q)/*Quoted[Query[Person]]*/.filter(p => p.isRussian))
    inline def q1 = quote(q.filter(p => p.isRussian))
    inline def q2 = quote(q1.filter(p => p.isHuman))

    //PrintMac(q1)
    
    println(q2)
  }
}