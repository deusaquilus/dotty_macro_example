package org.stuff

import scala.quoted._

object ParseMac {
  inline def apply(inline anyRaw: Any): Unit = ${ parseImpl('anyRaw) }
  def parseImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given _}
    val unsealUtil = new UnsealUtil
    import unsealUtil.{qctx => _, _}
    val any = anyRaw.unseal.underlyingArgument.seal

    any match {
      case Unseal(Apply(Select(a, "=="), List(b))) =>
        println("============== YAY IT MATCHES =================")
        println(s"A is: ${pprint.apply(a)} and B is: ${pprint.apply(b)}")
      case _ =>
        println("~~~~~~~~~~~~~~~~~~~============= NO IT DOES NOT MATCH (let's show it anyway) =============~~~~~~~~~~~~~~~~~~~~")
        PrintMac.printImpl(any)
    }

    '{ () }
  }
}
