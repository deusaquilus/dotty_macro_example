package org.stuff

import scala.quoted._

object PrintMac {
  inline def apply(inline any: Any): Unit = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given _}

    object UntypeExpr {
      def unapply(expr: Expr[_]): Option[Expr[_]] = 
        Untype.unapply(expr.unseal).map(_.seal)

      def apply(expr: Expr[_]): Expr[_] = Untype.unapply(expr.unseal).map(_.seal).get
    }

    // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
    object Untype {
      def unapply(term: Term): Option[Term] = term match {
        case TypedMatroshkaTerm(t) => Some(t)
        case other => Some(other)
      }

      def apply(term: Term): Term = Untype.unapply(term).get
    }

    object TypedMatroshkaTerm {
      def recurse(innerTerm: Term): Term = innerTerm match {
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other
      }

      def unapply(term: Term): Option[Term] = term match {
        case Typed(tree, _) => Some(recurse(tree))
        case other => None
      }
    }

    val parsedExpr = 
      expr.show
      .replace("org.stuff.", "")
      .replace("UseSimpleQuill2.", "")
      .replace(".apply", "")

    println("================== The Short-Short Version ================")
    println(CodeFormatter.apply(s"object Foo { ${parsedExpr} }"))
    println("================== The Short Version ================")
    println(CodeFormatter.apply(s"object Foo { ${expr.show} }"))
    println("================== The Long Version ================")
    println(pprint.apply(expr.unseal.underlyingArgument))
    println("================== Extractors ================")
    // If you want to remove the Typed(...) instances from your ast
    //println(CodeFormatter.apply(s"object Foo { ${Untype(expr.unseal.underlyingArgument).showExtractors} }"))
    println(CodeFormatter.apply(s"object Foo { ${expr.unseal.underlyingArgument.showExtractors} }"))
    '{ () }
  }
}
