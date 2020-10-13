package org.stuff

import io.getquill.ast._

object ToyDsl {
  class NonQuotedException extends Exception(NonQuotedException.message)

  object NonQuotedException {
    final val message = "The query definition must happen within a `quote` block."
    def apply() = throw new NonQuotedException
  }

  sealed trait JoinQuery[A, B, R] extends Query[R] {
    def on(f: (A, B) => Boolean): Query[R] = NonQuotedException()
  }


  sealed trait Query[+T] {

    def map[R](f: T => R): Query[R] = NonQuotedException()

    def flatMap[R](f: T => Query[R]): Query[R] = NonQuotedException()

    def concatMap[R, U](f: T => U)(implicit ev: U => Iterable[R]): Query[R] = NonQuotedException()

    def withFilter(f: T => Boolean): Query[T] = NonQuotedException()
    def filter(f: T => Boolean): Query[T] = NonQuotedException()

    //def sortBy[R](f: T => R)(implicit ord: Ord[R]): Query[T] = NonQuotedException()

    def take(n: Int): Query[T] = NonQuotedException()
    def drop(n: Int): Query[T] = NonQuotedException()

    def ++[U >: T](q: Query[U]): Query[U] = NonQuotedException()
    def unionAll[U >: T](q: Query[U]): Query[U] = NonQuotedException()
    def union[U >: T](q: Query[U]): Query[U] = NonQuotedException()

    def groupBy[R](f: T => R): Query[(R, Query[T])] = NonQuotedException()

    def value[U >: T]: Option[T] = NonQuotedException()
    def min[U >: T]: Option[T] = NonQuotedException()
    def max[U >: T]: Option[T] = NonQuotedException()
    def avg[U >: T](implicit n: Numeric[U]): Option[BigDecimal] = NonQuotedException()
    def sum[U >: T](implicit n: Numeric[U]): Option[T] = NonQuotedException()
    def size: Long = NonQuotedException()

    def join[A >: T, B](q: Query[B]): JoinQuery[A, B, (A, B)] = NonQuotedException()
    def leftJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (A, Option[B])] = NonQuotedException()
    def rightJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (Option[A], B)] = NonQuotedException()
    def fullJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (Option[A], Option[B])] = NonQuotedException()

    def join[A >: T](on: A => Boolean): Query[A] = NonQuotedException()
    def leftJoin[A >: T](on: A => Boolean): Query[Option[A]] = NonQuotedException()
    def rightJoin[A >: T](on: A => Boolean): Query[Option[A]] = NonQuotedException()

    def nonEmpty: Boolean = NonQuotedException()
    def isEmpty: Boolean = NonQuotedException()
    def contains[B >: T](value: B): Boolean = NonQuotedException()

    def distinct: Query[T] = NonQuotedException()

    def nested: Query[T] = NonQuotedException()

    //def foreach[A <: Action[_], B](f: T => B)(implicit unquote: B => A): BatchAction[A] = NonQuotedException()
  }

  def querySchema[T](entity: String, columns: (T => (Any, String))*): EntityQuery[T] = ???

  trait EntityQuery[T] extends Query[T] {

  override def withFilter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  override def filter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  override def map[R](f: T => R): EntityQuery[R] = NonQuotedException()

  // def insert(value: T): Insert[T] = NonQuotedException()
  // def insert(f: (T => (Any, Any)), f2: (T => (Any, Any))*): Insert[T] = NonQuotedException()

  // def update(value: T): Update[T] = NonQuotedException()
  // def update(f: (T => (Any, Any)), f2: (T => (Any, Any))*): Update[T] = NonQuotedException()

  // def delete: Delete[T] = NonQuotedException()
}

}

object UseSimpleQuill3 {
  case class Person(name:String, age:Int)

  def main(args: Array[String]):Unit = {
    import ToyDsl._
    inline def q = querySchema[Person]("tblPerson", _.name -> "colName", _.age -> "colAge")
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

