package org.stuff

import scala.quoted._
import io.getquill.ast._

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

  inline def run[T](inline qRaw: Quoted[Query[T]]) = ${ runImpl[T]('qRaw) }
  def runImpl[T:Type](qRaw: Expr[Quoted[Query[T]]])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given _}
    val q = qRaw.unseal.underlyingArgument.seal
    
    val unlifter = new Unlifter
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

          case _ => qctx.throwError(
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


  class Unlifter(implicit qctx: QuoteContext) {
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}

    val unsealUtil = new UnsealUtil
    import unsealUtil._

    def unlift(op: Expr[BinaryOperator]): BinaryOperator = {
      import EqualityOperator.{`==` => ee, `!=` => ne}
      import BooleanOperator._
      import NumericOperator._
      import StringOperator.`startsWith`
      import SetOperator.`contains`

      op match {
        case '{ EqualityOperator.`==` } => EqualityOperator.`==`
        case '{ EqualityOperator.`!=` } => EqualityOperator.`!=`
        case '{ BooleanOperator.`&&` } => `&&` 
        case '{ BooleanOperator.`||` } => `||`
        case '{ NumericOperator.`>`  } => `>` 
        case '{ NumericOperator.`>=` } => `>=` 
        case '{ NumericOperator.`<`  } => `<` 
        case '{ NumericOperator.`<=` } => `<=`
        case '{ StringOperator.`+` } => StringOperator.`+`
        case '{ StringOperator.`startsWith` } => `startsWith`
        case '{ SetOperator.`contains` } => `contains`
        case _ => qctx.throwError(s"Not consider operator (in unlift): ${op}, will add later")
      }
    }

    def apply(expr: Expr[Ast]): Ast =
      expr match {
        case '{ Entity(${Unseal(Literal(TConstant(name: String)))}: String, $list) } => Entity(name, List())
        // Filter(inside: Ast, id: Ident, body: Ast)
        // Filter(Entity("Person"), Ident("p"), Property(Ident("p"), "isSober"))
        case '{ Filter($queryAst, $idAst, $propertyAst) } =>
          val query: Ast = apply(queryAst)
          val id: Ident = apply(idAst).asInstanceOf[Ident]
          val prop: Ast = apply(propertyAst)
          Filter(query, id, prop)

        case '{ Constant.apply(${Unseal(Literal(TConstant(value: String)))}) } => 
          Constant(value)

        case '{ Ident(${Unseal(Literal(TConstant(name: String)))}) } => Ident(name)
        // Property(inside: Ast, name: String)
        // Property(Property(Ident("person"), "name"), "firstName")  person.name.firstName
        // Property(Ident("person"), "isSober") person.isSober
        case '{ Property($insideAst, ${Unseal(Literal(TConstant(name: String)))}) } => 
          val inside = apply(insideAst)
          Property(inside, name)
        
        

        case '{BinaryOperation($aExpr, $binaryOpExpr, $bExpr)} =>
          val a = apply(aExpr)
          val binaryOp = unlift(binaryOpExpr)
          val b = apply(bExpr)
          BinaryOperation(a, binaryOp, b)


        case _ => 
          qctx.throwError(
          s"""|Cannot unlift the tree: 
              |=================== Simple ==============
              |${CodeFormatter.apply(s"object Foo { ${expr.show} }")}
              |=================== Full AST ==============
              |${pprint.apply(expr.unseal)}
              |=================== Use Extractors ==============
              |${CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }")}
              """.stripMargin)
      }
  }

  class UnsealUtil(implicit val qctx: QuoteContext) {
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}

    object Unseal {
      def unapply(expr: Expr[_]): Option[Term] = Some(expr.unseal)
    }

    object Seal {
      def unapply(term: Term): Option[Expr[Any]] = Some(term.seal)
    }
  }

  inline def quote[T](inline quoted:T): Quoted[T] = ${ quoteImpl[T]('quoted) }
  def quoteImpl[T:Type](quoted: Expr[T])(implicit qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}

    val unlifter = new Unlifter
    import unlifter._
    val unsealUtil = new UnsealUtil
    import unsealUtil.{qctx => _, _}

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

    // Parser Starts here
    object Parser {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case Unseal(Apply(Select(Seal(a), "=="), List(Seal(b)))) =>
           BinaryOperation(astParse(a), EqualityOperator.`==`, astParse(b))

          case Unseal(Apply(Select(Seal(a), "!="), List(Seal(b)))) =>
            BinaryOperation(astParse(a), EqualityOperator.`!=`, astParse(b))

          // same as scala.quoted.matchers.Const
          case Unseal(Literal(TConstant(str: String))) => Constant(str)

          case '{ ($q: Quoted[$t]).unquote } =>
            astParse(q)

          case '{ Quoted.apply[$t]($ast) } =>
            unlifter.apply(ast)

          case '{ Dsl.query[$t] } => 
            val name = t.unseal.tpe.classSymbol.get.name
            Entity(name, List())

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
            |=================== Use Extractors ==============
            |${CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }")}
            """.stripMargin
          )
        }
      }
    }

    val quillAst: Ast = Parser.astParse(quotedRaw)
    //println(pprint.apply(quillAst))

    object Lifter {
        import BooleanOperator._
        import NumericOperator._
        import StringOperator.`startsWith`
        import SetOperator.`contains`
        import StringOperator.`+`

      def lift(op: BinaryOperator): Expr[BinaryOperator] = {
        op match {
          case _ if (op == EqualityOperator.`==`) => '{ EqualityOperator.`==` }
          case _ if (op == EqualityOperator.`!=`) => '{ EqualityOperator.`!=` }
          case `&&`  => '{ BooleanOperator.`&&` }
          case `||` => '{ BooleanOperator.`||` }
          case `>`  => '{ NumericOperator.`>`  }
          case `>=`  => '{ NumericOperator.`>=` }
          case `<`  => '{ NumericOperator.`<`  }
          case `<=` => '{ NumericOperator.`<=` }
          case `+` => '{ StringOperator.`+` }
          case `startsWith` => '{ StringOperator.`startsWith` }
          case `contains` => '{ SetOperator.`contains` }
          case _ => qctx.throwError(s"Not consider operator: ${op}, will add later")
        }
      }

      def apply(ast: Ast): Expr[Ast] =
        ast match {
          case Entity(name, _)            => '{ Entity(${Expr(name)}, List()) }
          case Filter(query, alias, body) => '{ Filter(${Lifter(query)}, ${Lifter.apply(alias).asInstanceOf[Expr[Ident]] /* must be Expr[Ident] */}, ${Lifter(body)}) }
          case Ident(id:String)           => '{ Ident(${Expr(id)}) }
          case Property(id, name)         => '{ Property(${Lifter(id)}, ${Expr(name)}) }
          case Constant(value: String)    => '{ Constant(${Expr(value)}) }
          case BinaryOperation(a, binaryOp,b) =>
            val aExpr = apply(a)
            val binaryOpExpr = lift(binaryOp)
            val bExpr = apply(b)
            '{BinaryOperation($aExpr, $binaryOpExpr, $bExpr)}
            
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

