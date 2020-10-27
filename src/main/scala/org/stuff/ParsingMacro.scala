package org.stuff

import scala.quoted._

object ParseMac {
  inline def apply(inline anyRaw: Any): Unit = ${ parseImpl('anyRaw) }
  def parseImpl(anyRaw: Expr[Any])(implicit qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given _}
    val unsealUtil = new UnsealUtil
    import unsealUtil.{qctx => _, _}
    val any = anyRaw.unseal.underlyingArgument.seal

    object Untype {
      def unapply(term: Term): Option[Term] = term match {
        case TypedMatroshkaTerm(t) => Some(t)
        case other => Some(other)
      }
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

    def isQueryType(t: Term): Boolean = {
      t.tpe.widen match {
        case (AppliedType(TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "io"),"getquill"),"Query"),x)) => true
        case _ => false
      }
    }

    def isQuery(tpe: Type) = {
      tpe <:< '[io.getquill.Query[Any]].unseal.tpe
    }
    import io.getquill.Query

    any match {
      //query[Person].union(query[Person])
      // Originally We Had:
      //case Unseal(Untype(Apply( TypeApply(Select(a, "union"), List(Inferred())), List(b) ))) if (isQueryType(a) && isQueryType(b)) =>
      // Then We Did:
      //case Unseal(Untype(Apply( TypeApply(Select(a, "union"), List(Inferred())), List(b) ))) if (isQuery(a.tpe) && isQuery(b.tpe)) =>
      // Finally We Got To:
      case '{ ($a: Query[$t]).union($b) } =>
      //case '{ type $t; ($a: Query[`$t`]).union(($b: Query[`$t`])) } =>

        println("============== YAY IT MATCHES =================")
        val t = a.unseal.tpe

        //val t: Type[Query] = '[ Query ]
        //val f: Expr[Foo] = '{ foo }
        
        // '[io.getquill.Query].unseal
        // AppliedType(TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "io"),"getquill"),"Query")
        // AppliedType(TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "io"),"getquill"),"Query"),x)

        // a.tpe.widen match {
        //   case (
        //     AppliedType(TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "io"),"getquill"),"Query"),x)
        //   ) => println("============= YAY IT MATCHES ================")
        //   case _ =>
        //     println("=========== NOPE DOESN'T MATCH ===============")
        // }


        println(s"The type should be something like ${CodeFormatter.applyInFoo('[io.getquill.Query[Any]].unseal.tpe.showExtractors)}")
        //println(s"a is: ${CodeFormatter.apply("object Foo {" + a.tpe.widen.showExtractors + "}")}")
        //println(s"b is: ${CodeFormatter.apply("object Foo {" + b.tpe.widen.showExtractors + "}")}")
        
      case _ =>
        println("~~~~~~~~~~~~~~~~~~~============= NO IT DOES NOT MATCH (let's show it anyway) =============~~~~~~~~~~~~~~~~~~~~")
        PrintMac.printImpl(any)
    }

    '{ () }
  }
}
