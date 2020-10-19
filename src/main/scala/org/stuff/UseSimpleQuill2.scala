// package org.stuff

// import Dsl._

// object UseSimpleQuill2 {

//   case class Person(name:String, isSober:Boolean, isRussian:Boolean, isHuman:Boolean)
//   implicit inline def autoUnquote[T](inline quoted: Quoted[T]): T = unquote(quoted)

//   def main(args: Array[String]):Unit = {
//     inline def q: Quoted[Query[Person]] = quote { 
//       query[Person].filter(p => p.name != "Joe") //  Query[Person] //hellooooooo
//     }
//     PrintMac(q) //hello

//     val v = List(1,2,3)
//     v.lift

//     // //inline def q1 = quote(unquote(q)/*Quoted[Query[Person]]*/.filter(p => p.isRussian))
//     inline def q1 = quote(q.filter(p => p.isRussian))

//     inline def q2 = quote(q1.filter(p => p.isHuman))

//     println(pprint.apply(q2.ast))

//     // //PrintMac(q1)
    
//     // println(pprint.apply(q2))
//   }
// }