package org.stuff

import scala.quoted._
import io.getquill.ast.{Query => AQuery, _}

// // case class Person(name:Name, age:Int)
// // case class Name(first:String, last:String)

// sealed trait Ast
// case class Entity(name:String) extends Ast
// case class Ident(name:String) extends Ast
// case class Filter(query:Ast, alias:Ident, body: Ast) extends Ast

// // val p = quote { query[Person].filter(p => body) }
// // val p = quote { query[Person].filter(p => p.name == "Joe") }
// case class Property(ast: Ast, name: String) extends Ast
// case class Constant(value:String) extends Ast

// trait Operator
// object Operator {
//   case object `==` extends Operator
// }
// case class BinaryOperation(left:Ast, op:Operator, right:Ast) extends Ast

// // p.name == "Joe"
// // BinaryOperation(Property(Ident(p), "name"), `==`, Constant("Joe"))



// TypeApply(Select(Ident(Dsl), query), List(Type[Person]))

object Dsl {
  def query[T]: Query[T] = throw new IllegalArgumentException("This can only be used inside a quoted block")

  inline def run[T](inline qRaw: Quoted[Query[T]]) = ${ runImpl[T]('qRaw) }
  def runImpl[T:Type](qRaw: Expr[Quoted[Query[T]]])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given _}
    val q = qRaw.unseal.underlyingArgument.seal
    
    val unlifter = new UnlifterEngine
    import unlifter._
    val unsealUtil = new UnsealUtil
    import unsealUtil.{qctx => _, _}

    object QuotedBlockPuller {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case '{ ($q: Quoted[$t]).unquote } =>
            astParse(q)

          case '{ Quoted.apply[$t]($ast) } =>
            unlifter.apply(ast)

          case Unseal(Typed(inside /*Term*/, _)) => astParse(inside.seal)

          case _ => report.throwError(
            s"""
            |Cannot run the tree: 
            |=================== Simple ==============
            |${CodeFormatter.apply(s"object Foo { ${expr.show} }")}
            |=================== Full AST ==============
            |${pprint.apply(expr.unseal)}
            |=================== Use Extractors ==============
            |${CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }")}
            """.stripMargin
          )
        }
      }
    }

    val ast = QuotedBlockPuller.astParse(q)
    
    println(
      "========================= YAY Printing AST ========================\n" +
      pprint.apply(ast)
    )
    val dialect = new io.getquill.PostgresDialect {}
    val (_, stmt) = dialect.translate(ast)(io.getquill.Literal)
    val sql = stmt.toString

    println("====================== HURRAH WE HAVE SQL =======================\n" + sql)

    Expr(sql)
  }

  inline def unquote[T](inline quoted:Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T:Type](quoted: Expr[Quoted[T]])(implicit qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{_, given _}
    '{ $quoted.unquote } /*Quoted[Query[T]] => Query[T]*/
  }



  inline def quote[T](inline quoted:T): Quoted[T] = ${ Quotation.apply[T]('quoted) }
  
    
}

/*
import Dsl._
case class Person(name:String, age:Int)
val q = quote { query[Person] }
*/

