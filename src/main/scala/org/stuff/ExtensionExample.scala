package org.stuff

object TheOldWay {
  trait Joe[T] {
    def isJoe(t: T): Boolean
  }
  
  implicit def stringJoe: Joe[String] = new Joe[String]() {
    def isJoe(t: String) = t.toLowerCase == "joe"
  }
  implicit def intJoe: Joe[Int] = new Joe[Int]() {
    def isJoe(t: Int) = t == 3
  }

  implicit class StringExt[T](something: T)(implicit someJoe: Joe[T]) {
    def isJoe: Boolean = someJoe.isJoe(something)
  }

  def main(args: Array[String]): Unit = {

    val someone: String = "Joe"
    println( someone.isJoe )
  }
}

object TheNewWay {

  trait Joe[T] {
    def isJoe(t: T): Boolean
  }
  
  given Joe[String] {
    def isJoe(t: String): Boolean = t.toLowerCase == "joe"
  }
  given Joe[Int] {
    override def isJoe(t: Int): Boolean = t == 3
  }

  extension [T](str:T)(using stringJoe: Joe[T]) {
    def isJoe: Boolean = stringJoe.isJoe(str)
  }

  def main(args: Array[String]): Unit = {

    val someone: String = "Joe"
    println( someone.isJoe )
  }

}


object TheNewWayMixed {

  trait Joe[T] {
    def isJoe(t: T): Boolean
  }

  implicit def stringJoe: Joe[String] = new Joe[String]() {
    def isJoe(t: String) = t.toLowerCase == "joe"
  }
  implicit def intJoe: Joe[Int] = new Joe[Int]() {
    def isJoe(t: Int) = t == 3
  }
  
  extension [T](str:T)(using stringJoe: Joe[T]) {
    def isJoe: Boolean = stringJoe.isJoe(str)
  }

  def main(args: Array[String]): Unit = {

    val someone: String = "Joe"
    println( someone.isJoe )
  }

}


object TheNewWayMixed2 {

  trait Joe[T] {
    def isJoe(t: T): Boolean
  }

  given Joe[String] {
    def isJoe(t: String): Boolean = t.toLowerCase == "joe"
  }
  given Joe[Int] {
    override def isJoe(t: Int): Boolean = t == 3
  }
  
  implicit class StringExt[T](something: T)(implicit someJoe: Joe[T]) {
    def isJoe: Boolean = someJoe.isJoe(something)
  }
  
  def main(args: Array[String]): Unit = {

    val someone: String = "Joe"
    println( someone.isJoe )
  }

}