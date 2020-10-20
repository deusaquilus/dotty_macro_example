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

    implicit def entityUnlifter: Unlifter[Entity] = new Unlifter[Entity] {
      def unlift(elem: Expr[Entity]): Entity = elem match {
        case '{ Entity(${Unseal(Literal(TConstant(name: String)))}: String, $list) } => Entity(name, List())
      }
    }
    implicit class HasUnlifter[T](elem: Expr[T])(implicit unlifter: Unlifter[T]) {
      def applyUnlift: T = unlifter.unlift(elem)
    }
  }
  import UnliftLibrary._

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
      case _ => report.throwError(s"Not consider operator (in unlift): ${op}, will add later")
    }
  }

  def apply(expr: Expr[Ast]): Ast =
    expr match {
      case '{ $e: Entity } => e.applyUnlift
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
        report.throwError(
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
