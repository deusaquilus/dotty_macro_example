package org.stuff

import scala.quoted._

object SimpleQuill1 {

  sealed trait Ast
  case class Entity(value:String) extends Ast

  // quote(query[Person])
  case class EntityQuery(value: String)

  inline def query[T]: EntityQuery = ${ queryImpl[T] }
  def queryImpl[T](implicit qctx: QuoteContext, tt: Type[T]): Expr[EntityQuery] = {
    import qctx.tasty.{_, given _}
    val className = tt.unseal.tpe.classSymbol.get.name
    '{ EntityQuery.apply(${Expr(className)}) }
  }

  case class Quoted[T](ast: Ast)
  inline def quote[T](inline code: T): Quoted[T] = ${ quoteImpl[T]('code) }
  def quoteImpl[T](code: Expr[T])(implicit qctx: QuoteContext, tt: Type[T]): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given _}

    object Unseal {
      def unapply[T](expr: Expr[T]) = Some(expr.unseal)
    }
    def unascribe(term: Term) =
      term match {
        case Typed(value, _) => value
        case other => other
      }

    val parsedClassName =
      unascribe(code.unseal.underlyingArgument).seal match {
        case '{ EntityQuery.apply(${Unseal(Literal(Constant(name:String)))}) } => name
      }
    '{ Quoted(Entity(${Expr(parsedClassName)})) }
  }

  // quote { query[Person] }
  // quote { EntityQuery("Person") }
  // Quoted(Entity("Person"))
  // run(Quoted(Entity("Person"))) => Select * from Person


  // quote { query[Person].filter(p => p.name == "Joe") }
  // quote { EntityQuery("Person").filter(p => p.name == "Joe") }
  // Quoted(Filter(Entity("Person"), Ident(p), Equals(Property(Ident(p), "name"), Constant("Joe"))
  // run(Filter(Entity("Person"), Ident(p), Equals(Property(Ident(p), "name"), Constant("Joe")) => 
  //  Select * from Person where p.name = 'Joe'

  inline def printTheTree(inline tree: Any):Any = ${ printTheTreeImpl('tree) }
  def printTheTreeImpl(tree: Expr[Any])(implicit qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{_, given _}
    println(tree.show)
    //println(io.getquill.util.Messages.qprint(tree.unseal.underlyingArgument))
    tree
  }

  inline def getMyTree(inline tree: String): String = ${ getMyTreeImpl('tree) }
  def getMyTreeImpl(tree: Expr[String])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given _}

    val unsealedTree = tree.unseal
    val output1: Term = 
      unsealedTree match {
        case Apply(Select(Literal(Constant(value:String)), "toUpperCase"), Nil) => Literal.apply(Constant.apply("Yay"))
        case _ => Literal(Constant("Nay"))
      }
    val thingThatWeCanReturn: Expr[String] = output1.seal.cast[String]

    val output =
      tree match {
        case '{ (${stringWeAreUpperCasing}: String).toUpperCase } =>
          '{ "Yay" }
        case _ =>
          '{ "Nay" }
      }

    object Unseal {
      def unapply[T](expr: Expr[T]) = Some(expr.unseal)
    }
    object MoreThen4Chars {
      def unapply(str:String) = if (str.length > 4) Some(str) else None
    }

    val output3 =
      tree match {
        // if (tree.unseal.isLiteral && tree.unseal.literal.isConstant && tree.unseal.literal.constant.length > 4 && tree.unseal.literal.constant.hasType(String) && tree.unseal.literal.constant.hasApply("toUpperCase"))
        case '{ (${Unseal(Literal(Constant(MoreThen4Chars(value))))}: String).toUpperCase } =>
          '{ "Yay its more then a 4 char constant" }
        case '{ (${Unseal(Literal(Constant(value)))}: String).toUpperCase } =>
          '{ "Yay its a constant" }
        case '{ (${Unseal(Literal(Constant(value)))}: String).toUpperCase } =>
          '{ "Yay its a string" }
        case _ =>
          '{ "Nay" }
      }

    val outputExpr = output3
    '{ ${outputExpr}.toUpperCase }
  }
}
