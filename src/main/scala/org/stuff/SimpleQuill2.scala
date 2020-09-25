package org.stuff

import scala.quoted._

// case class Person(name:Name, age:Int)
// case class Name(first:String, last:String)

sealed trait Ast
case class Entity(name:String) extends Ast
case class Ident(name:String) extends Ast
case class Filter(query:Ast, alias:Ident, body: Ast) extends Ast

// val p = quote { query[Person].filter(p => body) }
// val p = quote { query[Person].filter(p => p.name == "Joe") }
case class Property(ast: Ast, name: String) extends Ast
case class Constant(value:String) extends Ast

trait Operator
object Operator {
  case object `==` extends Operator
}
case class BinaryOperation(left:Ast, op:Operator, right:Ast) extends Ast

// p.name == "Joe"
// BinaryOperation(Property(Ident(p), "name"), `==`, Constant("Joe"))



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
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, _, given _}

    object Lambda1 {
      def unapplyTerm(term: Term): Option[(String, Term)] = term match {
        case Lambda(List(ValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
        case Block(List(), expr) => unapplyTerm(expr)
        case _ => None
      }

      def unapply(term: Expr[_]): Option[(String, Expr[_])] =
        unapplyTerm(term.unseal).map((str, term) => (str, term.seal))
    }

    // scala.quoted.Type[T] => TType[T]
    // qctx.tasty.Type
    //println("============= GOT INTO THE MACRO ==============")

    val t = summon[Type[T]]
    val quotedRaw = quoted.unseal.underlyingArgument.seal

    object Unseal {
      def unapply(expr: Expr[_]): Option[Term] = Some(expr.unseal)
    }

    // Parser Starts here
    object Parser {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case '{ Dsl.query[$t] } => 
            val name = t.unseal.tpe.classSymbol.get.name
            Entity(name)

          // inline def q = quote{ query[Person] }
          // quote { q.filter(p => p.name) }
          // Filter(Ident(q), Ident(p), Property(Ident(p), "name"))

          // def filter(e:T => Boolean): Query[T]
          case '{ ($query: Query[$t]).filter(${Lambda1(alias, body)}) } =>
            Filter(astParse(query), Ident(alias), astParse(body))

          // p.name - Expr[T]
          // Select(Ident(id), prop) - Term

          case Unseal(Select(TIdent(id: String), prop)) =>
            Property(Ident(id), prop)

          case _ => qctx.throwError(s"Cannot parse the tree: ${expr.show}")
        }
      }
    }

    //println("============== GOT TO PARSER =============")
    val quillAst: Ast = Parser.astParse(quotedRaw)

    def foo(bar:String): Expr[String] = ???

    object Lifter {
      def apply(ast: Ast): Expr[Ast] =
        ast match {
          case Entity(name) => 
            '{ Entity(${Expr(name)}) }
          case Filter(query, alias, body) =>
            // case class Filter(query:Ast, alias:Ident, body:Ast)
            '{ Filter(${Lifter(query)}, ${Lifter.apply(alias).asInstanceOf[Expr[Ident]] /* must be Expr[Ident] */}, ${Lifter(body)}) }
          case Ident(id:String) => // String => Expr[String]
            '{ Ident(${Expr(id)}) } // '{ Ident(id: Expr[String]) } is actually Expr[Ast]
          case Property(id, name) =>
            '{ Property(${Lifter(id)}, ${Expr(name)}) }
          case _ => 
            qctx.throwError(s"Cannot lift the tree:\n${pprint.apply(ast)}")
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

