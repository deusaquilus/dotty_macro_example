package org.stuff

import io.getquill.ast.Ast

case class Quoted[T](ast: Ast) {
  def unquote = throw new IllegalArgumentException("Only a compile-time-construct")
}
