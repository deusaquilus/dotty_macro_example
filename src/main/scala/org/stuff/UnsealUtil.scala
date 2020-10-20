package org.stuff

import scala.quoted.{Expr, QuoteContext}

class UnsealUtil(implicit val qctx: QuoteContext) {
  import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}

  object Unseal {
    def unapply(expr: Expr[_]): Option[Term] = Some(expr.unseal)
  }

  object Seal {
    def unapply(term: Term): Option[Expr[Any]] = Some(term.seal)
  }
}
