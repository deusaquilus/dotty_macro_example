package org.stuff

import scala.quoted._

object PrintMac {
  inline def apply(inline any: Any): Unit = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given _}
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
    println(CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }"))
    '{ () }
  }
}
