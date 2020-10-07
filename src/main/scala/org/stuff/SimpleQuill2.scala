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



case class Quoted[T](ast: Ast) {
  def unquote = throw new IllegalArgumentException("Only a compile-time-construct")
}

class Query[T] {
  def filter(e:T => Boolean): Query[T] 	    = throw new IllegalArgumentException("This can only be used inside a quoted block")
  def map[R](e:T => R): Query[R]     			  = throw new IllegalArgumentException("This can only be used inside a quoted block")
  def flatMap[R](e:T => Query[R]): Query[R] = throw new IllegalArgumentException("This can only be used inside a quoted block")
}

// TypeApply(Select(Ident(Dsl), query), List(Type[Person]))

object Dsl {
  def query[T]: Query[T] = throw new IllegalArgumentException("This can only be used inside a quoted block")

  inline def unquote[T](inline quoted:Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T:Type](quoted: Expr[Quoted[T]])(implicit qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{_, given _}
    '{ $quoted.unquote } /*Quoted[Query[T]] => Query[T]*/
  } 

  inline def quote[T](inline quoted:T): Quoted[T] = ${ quoteImpl[T]('quoted) }
  def quoteImpl[T:Type](quoted: Expr[T])(implicit qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}

    object Lambda1 {
      def unapplyTerm(term: Term): Option[(String, Term)] = term match {
        case Lambda(List(ValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
        case Block(List(), expr) => unapplyTerm(expr)
        case _ => None
      }

      def unapply(term: Expr[_]): Option[(String, Expr[_])] =
        unapplyTerm(term.unseal).map((str, term) => (str, term.seal))
    }

    val quotedRaw = quoted.unseal.underlyingArgument.seal

    object Unseal {
      def unapply(expr: Expr[_]): Option[Term] = Some(expr.unseal)
    }

    
    object Unlifter {
      def apply(expr: Expr[Ast]): Ast =
        expr match {
          case '{ Entity(${Unseal(Literal(TConstant(name: String)))}: String) } => Entity(name)
          // Filter(inside: Ast, id: Ident, body: Ast)
          // Filter(Entity("Person"), Ident("p"), Property(Ident("p"), "isSober"))
          case '{ Filter($queryAst, $idAst, $propertyAst) } =>
            val query: Ast = Unlifter(queryAst)
            val id: Ident = Unlifter(idAst).asInstanceOf[Ident]
            val prop: Ast = Unlifter(propertyAst)
            Filter(query, id, prop)

          case '{ Ident(${Unseal(Literal(TConstant(name: String)))}) } => Ident(name)
          // Property(inside: Ast, name: String)
          // Property(Property(Ident("person"), "name"), "firstName")  person.name.firstName
          // Property(Ident("person"), "isSober") person.isSober
          case '{ Property($insideAst, ${Unseal(Literal(TConstant(name: String)))}) } => 
            val inside = Unlifter.apply(insideAst)
            Property(inside, name)
          case _ => 
            qctx.throwError(s"Cannot unlift the tree:\n${pprint.apply(expr)}")
        }
    }

    // Parser Starts here
    object Parser {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case '{ ($q: Quoted[$t]).unquote } =>
            astParse(q)

          case '{ Quoted.apply[$t]($ast) } =>
            Unlifter(ast)

          case '{ Dsl.query[$t] } => 
            val name = t.unseal.tpe.classSymbol.get.name
            Entity(name)

          case '{ ($query: Query[$t]).filter(${Lambda1(alias, body)}) } =>
            Filter(astParse(query), Ident(alias), astParse(body))

          case Unseal(Select(TIdent(id: String), prop)) =>
            Property(Ident(id), prop)

          case Unseal(Typed(inside /*Term*/, _)) => astParse(inside.seal)

          case _ => qctx.throwError(
            s"""
            |Cannot parse the tree: 
            |=================== Simple ==============
            |${expr.show}
            |=================== Full AST ==============
            |${pprint.apply(expr.unseal)}
            """.stripMargin
          )
        }
      }
    }

    val quillAst: Ast = Parser.astParse(quotedRaw)
    //println(pprint.apply(quillAst))

    object Lifter {
      def apply(ast: Ast): Expr[Ast] =
        ast match {
          case Entity(name)               => '{ Entity(${Expr(name)}) }
          case Filter(query, alias, body) => '{ Filter(${Lifter(query)}, ${Lifter.apply(alias).asInstanceOf[Expr[Ident]] /* must be Expr[Ident] */}, ${Lifter(body)}) }
          case Ident(id:String)           => '{ Ident(${Expr(id)}) }
          case Property(id, name)         => '{ Property(${Lifter(id)}, ${Expr(name)}) }
          case _ => 
            qctx.throwError(s"Cannot lift the tree:\n${pprint.apply(ast)}")
        }
    }




    val liftedQuillAst: Expr[Ast] = Lifter.apply(quillAst)

    /*
    // query[Person].filter(p => p.isSober)
    // ===========================================
    Quoted[Query[UseSimpleQuill2.Person]](
      Filter(Entity("Person"), 
        Ident("p"), 
        Property(Ident("p"), "isSober")
      )
    )

    */
    '{ Quoted($liftedQuillAst) }
  }
}

/*
import Dsl._
case class Person(name:String, age:Int)
val q = quote { query[Person] }
*/

