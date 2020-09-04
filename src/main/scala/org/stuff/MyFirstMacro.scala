package org.stuff

import scala.quoted._

object MyFirstMacro {
  
  // ' - quote
  // $ - splice
  inline def myMac(inline foo: String): String = ${ myMacImpl('foo) }
  def myMacImpl(foo: Expr[String])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{ _, given _ }
    println("Mac1: Now we are showing:\n" + io.getquill.util.Messages.qprint(foo.unseal.underlyingArgument))

    def parseFunction(expr: Term): Option[String] = {
      expr match {
        case Typed(value, tpe) => parseFunction(value)
        case Literal(Constant(value: String)) => Some(value)
        case other => None
      }
    }

    val compileTimeString: Option[String] = parseFunction(foo.unseal.underlyingArgument)
    compileTimeString match {
      case Some(value) => println(s"Mac1: The expression: ${foo.show} is a compile-time string: ${compileTimeString}")
      case None => println(s"Mac1: The expression ${foo.show} is not a compile-time string")
    }

    foo
  }

  inline def myMac2(inline fooo: String): String = ${ myMacImpl2('fooo) }
  def myMacImpl2(fooo: Expr[String])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{ _, given _ }
    println("Mac2: Now we are showing:\n" + io.getquill.util.Messages.qprint(fooo.unseal.underlyingArgument))

    def parseFunction(expr: Term): Option[String] = {
      expr match {
        case Typed(value, tpe) => parseFunction(value)
        case Literal(Constant(value: String)) => Some(value)
        case other => None
      }
    }

    val compileTimeString: Option[String] = parseFunction(fooo.unseal.underlyingArgument)
    compileTimeString match {
      case Some(value) => println(s"Mac2: The expression: ${fooo.show} is a compile-time string: ${compileTimeString}")
      case None => println(s"Mac2: The expression ${fooo.show} is not a compile-time string")
    }

    fooo
  }
}
