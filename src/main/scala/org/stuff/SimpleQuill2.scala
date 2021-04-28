package org.stuff

import scala.quoted._

sealed trait Ast
case class Entity(name:String) extends Ast
case class Ident(name:String) extends Ast
case class Filter(query:Ast, alias:Ident, body: Ast) extends Ast
case class Map(query:Ast, alias:Ident, body: Ast) extends Ast
case class FlatMap(query:Ast, alias:Ident, body: Ast) extends Ast
case class Property(ast: Ast, name: String) extends Ast
case class Constant(value:String) extends Ast
case class Tuple(values: List[Ast]) extends Ast
case class Constant(value: Any) extends Ast
case class Function(params: List[Ident], body: Ast) extends Ast
case class FunctionApply(function: Ast, values: List[Ast]) extends Ast

trait Operator
object Operator {
  case object `==` extends Operator
}
case class BinaryOperation(left:Ast, op:Operator, right:Ast) extends Ast

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

  inline def unquote[T](inline quoted:Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T:Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
    import quotes.reflect._
    '{ $quoted.unquote } /*Quoted[Query[T]] => Query[T]*/
  } 

  inline def quote[T](inline quoted:T): Quoted[T] = ${ quoteImpl[T]('quoted) }
  def quoteImpl[T:Type](quoted: Expr[T])(using Quotes): Expr[Quoted[T]] = {
    import quotes.reflect.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _}

    object Lambda1:
      def unapplyTerm(term: Term): Option[(String, Term)] = 
        term match
          case Lambda(List(ValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
          case Block(List(), expr) => unapplyTerm(expr)
          case _ => None

      def unapply(term: Expr[_]): Option[(String, Expr[_])] =
        unapplyTerm(term.asTerm).map((str, term) => (str, term.seal))
    end Lambda1

    val quotedRaw = quoted.asTerm.underlyingArgument.seal

    object Unseal:
      def unapply(expr: Expr[_]): Option[Term] = Some(expr.asTerm)

    /** =========================== Parse ======================== **/
    object Parser {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case '{ ($q: Quoted[$t]).unquote } => astParse(q)
          case '{ Quoted.apply[$t]($ast) } => Unlifter(ast)
          case '{ Dsl.query[$t] } => Entity(t.asTerm.tpe.classSymbol.get.name)
          case '{ ($query: Query[$t]).filter(${Lambda1(alias, body)}) } => Filter(astParse(query), Ident(alias), astParse(body))


          case Unseal(Select(TIdent(id: String), prop)) => Property(Ident(id), prop)
          case Unseal(Typed(inside /*Term*/, _)) => astParse(inside.seal)
          case _ => qctx.throwError(
            s"""
            |Cannot parse the tree: 
            |=================== Simple ==============
            |${expr.show}
            |=================== Full AST ==============
            |${pprint.apply(expr.asTerm)}
            """.stripMargin
          )
        }
      }
    }

    val quillAst: Ast = Parser.astParse(quotedRaw)
    val liftedQuillAst: Expr[Ast] = Lifter(quillAst)
    '{ Quoted($liftedQuillAst) }
  }
}


/** =========================== Unlift ======================== **/
object Unlifter:
  def apply(ast: Expr[Ast]): Quotes ?=> Ast = unliftAst.apply(ast) // can also do ast.lift but this makes some error messages simpler

  extension [T](t: Expr[T])(using FromExpr[T], Quotes)
    def unexpr: T = t.valueOrError

  trait NiceUnliftable[T: ClassTag] extends FromExpr[T]:
    def unlift: Quotes ?=> PartialFunction[Expr[T], T]
    def apply(expr: Expr[T])(using Quotes): T = unlift.lift(expr).getOrElse { throw new IllegalArgumentException(s"Could not Unlift AST type into the Quill Abstract Syntax Tree") }
    def unapply(expr: Expr[T])(using Quotes): Option[T] = unlift.lift(expr)

  given unliftProperty: NiceUnliftable[Property] with
    def unlift =
      case '{ Property(${ast}, ${name}) } => Property(ast.unexpr, constString(name))

  given unliftAst: NiceUnliftable[Ast] with
    def unlift =
      case '{ Constant(${Expr(b: Double)}: Double) } => Constant(b)
      case '{ Constant(${Expr(b: Boolean)}: Boolean) } => Constant(b)
      case '{ Constant(${Expr(b: String)}: String) } => Constant(b)
      case '{ Constant(${Expr(b: Int)}: Int) } => Constant(b)
      case '{ Entity.apply(${Expr(b: String)})  } => Entity(b)
      case '{ Function($params, $body) } => Function(params.unexpr, body.unexpr)
      case '{ FunctionApply($function, $values) } => FunctionApply(function.unexpr, values.unexpr)
      case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(query.unexpr, alias.unexpr, body.unexpr)
      case '{ FlatMap(${query}, ${alias}, ${body}: Ast) } => FlatMap(query.unexpr, alias.unexpr, body.unexpr)
      case '{ Filter(${query}, ${alias}, ${body}: Ast) } => Filter(query.unexpr, alias.unexpr, body.unexpr)
      case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(a.unexpr, unliftOperator(operator).asInstanceOf[BinaryOperator], b.unexpr)
      case '{ Property(${ast}, ${Expr(name: String)}) } => Property(ast.unexpr, name)
      case '{ Tuple.apply($values) } => Tuple(values.unexpr)
      case '{ $p: Property } => unliftProperty(p)
      case '{ $id: Ident } => unliftIdent(id)

  given unliftOperator: NiceUnliftable[Operator] with
    def unlift = case '{ Operator.== } =>  Operator.==
end Unlifter

/** =========================== Lift ======================== **/
object Lifter:
  def apply(ast: Ast): Quotes ?=> Expr[Ast] = liftableAst(ast)

  extension [T](t: T)(using ToExpr[T], Quotes)
    def expr: Expr[T] = Expr(t)

  trait NiceLiftable[T: ClassTag] extends ToExpr[T]:
    def lift: Quotes ?=> PartialFunction[T, Expr[T]]
    def apply(t: T)(using Quotes): Expr[T] = lift.lift(t).getOrElse { throw new IllegalArgumentException(s"Could not Lift AST type into the Quill Abstract Syntax Tree") }
    def unapply(t: T)(using Quotes) = Some(apply(t))

  given liftableProperty : NiceLiftable[Property] with
    def lift =
      case Property(core: Ast, name: String) => '{ Property.Opinionated(${core.expr}, ${name.expr}) }

  given liftableIdent : NiceLiftable[Ident] with
    def lift =
      case Ident(name: String, quat) => '{ AIdent(${name.expr}, ${quat.expr})  }

  extension [T: TType](list: List[T])(using ToExpr[T], Quotes)
    def spliceVarargs = Varargs(list.map(Expr(_)).toSeq)

  given liftableEntity : NiceLiftable[Entity] with
    def lift = 
      case Entity(name: String, list, quat) => '{ Entity(${name.expr}, ${list .expr}, ${quat.expr})  }

  given liftableTuple: NiceLiftable[Tuple] with
    def lift = 
      case Tuple(values) => '{ Tuple(${values.expr}) }

  given liftableAst : NiceLiftable[Ast] with
    def lift =
      case Constant(tmc.ConstantValue(v), quat) => '{ Constant(${tmc.ConstantExpr(v)}, ${quat.expr}) }
      case Function(params: List[AIdent], body: Ast) => '{ Function(${params.expr}, ${body.expr}) }
      case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.expr}, ${values.expr}) }
      case v: Entity => liftableEntity(v)
      case v: Tuple => liftableTuple(v)
      case Map(query: Ast, alias: AIdent, body: Ast) => '{ Map(${query.expr}, ${alias.expr}, ${body.expr})  }
      case FlatMap(query: Ast, alias: AIdent, body: Ast) => '{ FlatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
      case Filter(query: Ast, alias: AIdent, body: Ast) => '{ Filter(${query.expr}, ${alias.expr}, ${body.expr})  }
      case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${a.expr}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${b.expr})  }
      case v: Property => liftableProperty(v)
      case v: Ident => liftableIdent(v)

  import Operator.{ == => ee}
  given liftOperator : NiceLiftable[Operator] with
    def lift =
      case _: ee.type => '{ EqualityOperator.== }
end Lifter
