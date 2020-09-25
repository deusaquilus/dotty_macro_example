package org.stuff

import scala.quoted._

object PrintMac {
  inline def apply(inline any: Any): Unit = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given _}
    println("================== The Short Version ================")
    println(expr.show)
    println("================== The Long Version ================")
    println(pprint.apply(expr.unseal.underlyingArgument))
    '{ () }
  }
}
