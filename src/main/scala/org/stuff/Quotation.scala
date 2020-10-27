package org.stuff

import scala.quoted._
import io.getquill.ast.{Query => AQuery, _}
import io.getquill._

object Quotation {
  def apply[T:Type](quoted: Expr[T])(implicit qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{Type => TTYpe, Ident => TIdent, Constant => TConstant, _, given _}
    
    val unlifter = new UnlifterEngine
    import unlifter._
    val unsealUtil = new UnsealUtil
    import unsealUtil.{qctx => _, _}

    object Lambda1 {
      def unapplyTerm(term: Term): Option[(String, Term)] = term match {
        case Lambda(List(ValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
        case Block(List(), expr) => unapplyTerm(expr)
        case _ => None
      }

      def unapply(term: Expr[_]): Option[(String, Expr[_])] =
        unapplyTerm(term.unseal).map((str, term) => (str, term.seal))
    }

    val quotedRaw = quoted.unseal.underlyingArgument.seal

    object QueryParser {
      def astParse(expr: Expr[Any]): Ast = expr match {
        case '{ ($query: Query[$t]).filter(${Lambda1(alias, body)}) } =>
          Filter(astParse(query), Ident(alias), astParse(body))
      }
    }

    object OperationParser {
      def astParse(expr: Expr[Any]): Ast = expr match {
        case Unseal(Apply(Select(Seal(a), "=="), List(Seal(b)))) =>
          BinaryOperation(astParse(a), EqualityOperator.`==`, astParse(b))

        case Unseal(Apply(Select(Seal(a), "!="), List(Seal(b)))) =>
          BinaryOperation(astParse(a), EqualityOperator.`!=`, astParse(b))
      }
    }

    // object FullParser = QueryParser.combine(OperationParser).combine(...).combine(...)

    // Parser Starts here
    object Parser {
      def astParse(expr: Expr[Any]): Ast = {
        expr match {
          case Unseal(Apply(Select(Seal(a), "=="), List(Seal(b)))) =>
            BinaryOperation(astParse(a), EqualityOperator.`==`, astParse(b))

          case Unseal(Apply(Select(Seal(a), "!="), List(Seal(b)))) =>
            BinaryOperation(astParse(a), EqualityOperator.`!=`, astParse(b))

          // same as scala.quoted.matchers.Const
          case Unseal(Literal(TConstant(str: String))) => Constant(str)

          case '{ ($q: Quoted[$t]).unquote } =>
            astParse(q)

          case '{ Quoted.apply[$t]($ast) } =>
            unlifter.apply(ast)

          case '{ Dsl.query[$t] } =>
            val name = t.unseal.tpe.classSymbol.get.name
            Entity(name, List())

          case '{ ($query: Query[$t]).filter(${Lambda1(alias, body)}) } =>
           Filter(astParse(query), Ident(alias), astParse(body))

          case Unseal(Select(TIdent(id: String), prop)) =>
            Property(Ident(id), prop)

          case Unseal(Typed(inside /*Term*/, _)) => astParse(inside.seal)

          case _ => report.throwError(
            s"""
               |Cannot parse the tree: 
               |=================== Simple ==============
               |${expr.show}
               |=================== Full AST ==============
               |${pprint.apply(expr.unseal)}
               |=================== Use Extractors ==============
               |${CodeFormatter.apply(s"object Foo { ${expr.unseal.showExtractors} }")}
            """.stripMargin
          )
        }
      }
    }

    val quillAst: Ast = Parser.astParse(quotedRaw)
    //println(pprint.apply(quillAst))

    trait Lifter[T] {
      def lift(element: T): Expr[T]
    }
    object Lifter {
      import BooleanOperator._
      import NumericOperator._
      import StringOperator.`startsWith`
      import SetOperator.`contains`
      import StringOperator.`+`

      implicit def binaryOperationLifter: Lifter[BinaryOperator] = new Lifter[BinaryOperator] {
        def lift(op: BinaryOperator): Expr[BinaryOperator] = {
          op match {
            case _ if (op == EqualityOperator.`==`) => '{ EqualityOperator.`==` }
            case _ if (op == EqualityOperator.`!=`) => '{ EqualityOperator.`!=` }
            case `&&`  => '{ BooleanOperator.`&&` }
            case `||` => '{ BooleanOperator.`||` }
            case `>`  => '{ NumericOperator.`>`  }
            case `>=`  => '{ NumericOperator.`>=` }
            case `<`  => '{ NumericOperator.`<`  }
            case `<=` => '{ NumericOperator.`<=` }
            case `+` => '{ StringOperator.`+` }
            case `startsWith` => '{ StringOperator.`startsWith` }
            case `contains` => '{ SetOperator.`contains` }
            case _ => report.throwError(s"Not consider operator: ${op}, will add later")
          }
        }
      }

      // trait Lifter[T] { 
      //   def lift(element: T): Expr[T]
      // }
      implicit def identLifter: Lifter[Ident] = new Lifter[Ident] {
        def lift(element: Ident): Expr[Ident] = element match {
          case Ident(id:String)           => '{ Ident(${Expr(id)}) }
        }
      }

      implicit val stringLifter: Lifter[String] = new Lifter[String] {
        def lift(str: String): Expr[String] = Expr(str)
      }

      // Given: there is a lifter on T
      // Therefore: there is a HasLifter class on T
      implicit class HasLifter[T](element: T)(implicit lifter: Lifter[T]) {
        def lift = lifter.lift(element)
        def liftA = lifter.lift(element)
      }



      // Given: You have a lifter of T
      // Therefore: We can give you a lifter of List[T]
      implicit def listLifter[T: Type](implicit elementLifter: Lifter[T]): Lifter[List[T]] = new Lifter[List[T]] {
        def lift(list: List[T]): Expr[List[T]] = {
          val listOfLifts = list.map(e => elementLifter.lift(e))
          // List[Expr[T]] => Expr[List[T]]
          // List[Container[Honey]] => Container[List[Honey]]
          Expr.ofList(listOfLifts /*Expr[T]*/)
        }
      }

      implicit def propertyAliasLifer: Lifter[PropertyAlias] = new Lifter[PropertyAlias] {
        def lift(propertyAlias: PropertyAlias): Expr[PropertyAlias] =
          propertyAlias match {
            case PropertyAlias(paths, alias) =>
              '{ PropertyAlias(${paths.liftA}, ${alias.lift}) }
          }
      }

      implicit def astLifter: Lifter[Ast] = new Lifter[Ast] {
        def lift(ast: Ast): Expr[Ast] =
          ast match {
            case id: Ident => id.lift
            case Entity(name, propertyAlias)            => '{ Entity(${name.lift}, ${propertyAlias.liftA}) }
            case Filter(query, alias, body) => '{ Filter(${query.lift}, ${alias.lift}, ${body.lift}) }
            case Property(inner, name)      => '{ Property(${inner.lift}, ${name.lift}) }
            case Constant(value: String)    => '{ Constant(${Expr(value)}) }
            case BinaryOperation(a, binaryOp,b) =>
              '{BinaryOperation(${a.lift}, ${binaryOp.lift}, ${b.lift})}

            case _ =>
              report.throwError(s"Cannot lift the tree:\n${pprint.apply(ast)}")
          }
      }

      def apply(ast: Ast): Expr[Ast] = ast.lift
    }




    val liftedQuillAst: Expr[Ast] = Lifter.apply(quillAst)

    /*
    // query[Person].filter(p => p.isSober)
    // ===========================================
    Quoted[Query[UseSimpleQuill2.Person]](
      Filter(Entity("Person"), 
        Ident("p"), 
        Property(Ident("p"), "isSober")
      )
    )

    */
    '{ Quoted($liftedQuillAst) }
  }
}