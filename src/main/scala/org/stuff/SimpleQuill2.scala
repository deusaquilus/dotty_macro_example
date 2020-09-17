package org.stuff

import scala.quoted._

sealed trait Ast
case class Entity(name:String) extends Ast

case class Quoted[T](ast: Ast)

class Query[T] {
  def filter(e:T => Boolean): Query[T] 	    = throw new IllegalArgumentException("This can only be used inside a quoted block")
  def map[R](e:T => R): Query[R]     			  = throw new IllegalArgumentException("This can only be used inside a quoted block")
  def flatMap[R](e:T => Query[R]): Query[R] = throw new IllegalArgumentException("This can only be used inside a quoted block")
}

// TypeApply(Select(Ident(Dsl), query), List(Type[Person]))

object Dsl {
  def query[T]: Query[T] = throw new IllegalArgumentException("This can only be used inside a quoted block")

  inline def quote[T](inline quoted:T): Quoted[T] = ${ quoteImpl[T]('quoted) }
  def quoteImpl[T:Type](quoted: Expr[T])(implicit qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{Type => TTYpe, _, given _}
    // scala.quoted.Type[T] => TType[T]
    // qctx.tasty.Type
    val t = summon[Type[T]]
    val quotedRaw = quoted.unseal.underlyingArgument.seal

    // Parser Starts here
    object Parser {
      def apply(expr: Expr[Any]): Ast = {
        expr match {
          case '{ Dsl.query[$t] } => 
            val name = t.unseal.tpe.classSymbol.get.name
            Entity(name)
        }
      }
    }

    val quillAst: Ast = Parser.apply(quotedRaw)

    object Lifter {
      def apply(ast: Ast): Expr[Ast] =
        ast match {
          case Entity(name) =>
            val nameExpression = Expr(name) 
            '{ Entity($nameExpression) }
        }
    }

    val liftedQuillAst = Lifter.apply(quillAst)

    '{ Quoted($liftedQuillAst) }
  }
}

/*
import Dsl._
case class Person(name:String, age:Int)
val q = quote { query[Person] }
*/

