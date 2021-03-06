package org.stuff

import scala.quoted._
import io.getquill.ast.{Query => AQuery, _}

class UnlifterEngine(implicit qctx: QuoteContext) {
  import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}
  val unsealUtil = new UnsealUtil
  import unsealUtil._

  object UnliftLibrary {
    trait Unlifter[T] {
      def unlift(elem: Expr[T]): T
    }
    object Unlifter {
      def Error[T](expr: Expr[T]) =
        report.throwError(
          s"""|Cannot unlift the tree: 
              |=================== Simple ==============
              |${CodeFormatter.apply(s"object Foo { ${expr.show} }")}
              |=================== Full AST ==============
              |${pprint.apply(expr.unseal)}
              |=================== Use Extractors ==============
              |${CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }")}
              """.stripMargin
        )
        
      def Make[T](pf: PartialFunction[Expr[T], T]) = 
        new Unlifter[T] {
          def unlift(expr: Expr[T]): T = 
            pf.lift(expr).getOrElse(Unlifter.Error(expr))
        }
    }

    given Unlifter[BinaryOperator] {
      
      import EqualityOperator.{`==` => ee, `!=` => ne}
      import BooleanOperator._
      import NumericOperator._
      import StringOperator.`startsWith`
      import SetOperator.`contains`

      def unlift(op: Expr[BinaryOperator]): BinaryOperator =
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
          case _ => Unlifter.Error(op)
        }
    }

    // Using a syntax with the Make function in the Unlifter object
    given Unlifter[Entity] = Unlifter.Make[Entity] {
      case '{ Entity(${Unseal(Literal(TConstant(name: String)))}: String, $list) } => Entity(name, List())
    }
    // same here
    given Unlifter[Ident] = Unlifter.Make[Ident] {
      case '{ Ident(${Unseal(Literal(TConstant(name: String)))}) } => Ident(name)
    }

    extension [T](elem: Expr[T])(using unlifter: Unlifter[T]) {
      def unlift: T = unlifter.unlift(elem)
    }

    given Unlifter[Ast] {
      def unlift(expr: Expr[Ast]): Ast =
        expr match {
          case '{ $id: Ident } => id.unlift
          case '{ $e: Entity } => e.unlift
          // Filter(inside: Ast, id: Ident, body: Ast)
          // Filter(Entity("Person"), Ident("p"), Property(Ident("p"), "isSober"))
          case '{ Filter($queryAst, $idAst, $propertyAst) } =>
            
            val query: Ast = queryAst.unlift
            val id: Ident = idAst.unlift
            val prop: Ast = propertyAst.unlift
            Filter(query, id, prop)

          case '{ Constant.apply(${Unseal(Literal(TConstant(value: String)))}) } =>
            Constant(value)

          
            // Property(inside: Ast, name: String)
            // Property(Property(Ident("person"), "name"), "firstName")  person.name.firstName
            // Property(Ident("person"), "isSober") person.isSober
          case '{ Property($insideAst, ${Unseal(Literal(TConstant(name: String)))}) } =>
            val inside = insideAst.unlift
            Property(inside, name)

          case '{BinaryOperation($aExpr, $binaryOpExpr, $bExpr)} =>
            val a = aExpr.unlift
            val binaryOp = binaryOpExpr.unlift
            val b = bExpr.unlift
            BinaryOperation(a, binaryOp, b)


          case _ => Unlifter.Error(expr)
        }
    }
  }
  import UnliftLibrary._

  def apply(expr: Expr[Ast]): Ast = expr.unlift

}
