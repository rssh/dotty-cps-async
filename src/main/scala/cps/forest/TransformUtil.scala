package cps.forest

import scala.quoted._

import cps._


object TransformUtil:


  def find(using qctx:QuoteContext)(term: qctx.tasty.Term,
                       cond: qctx.tasty.Tree=> Option[qctx.tasty.Tree]) :Option[qctx.tasty.Tree] = {
     import qctx.tasty.{_,given _}
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(using Owner): Option[Tree] =
                 foldOverTree(x,tree)

        override def foldOverTree(x: Option[Tree], tree: Tree)(using Owner): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse super.foldOverTree(x,tree)
        }
     }
     search.foldTree(None,term)
  }

  def containsAwait(using qctx:QuoteContext)(term: qctx.tasty.Term): Boolean =
    import qctx.tasty.{_,given _}
    find(term, {
           case v@Apply(TypeApply(id@Ident("await"),targs),args) =>
                         if (id.symbol.fullName == "cps.await") Some(v) else None
           case _ => None
         }).isDefined


  /**
   * substitute identifier with the origin symbol to new tree
   **/
  def substituteIdent(using qctx:QuoteContext)(tree: qctx.tasty.Term,
                           origin: qctx.tasty.Symbol,
                           newTerm: qctx.tasty.Term): qctx.tasty.Term =
     import qctx.tasty.{_,given _}
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(using Owner):Term =
          tree match
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newTerm
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


  def namedLet(using qctx: QuoteContext)(name: String, rhs: qctx.tasty.Term)(body: qctx.tasty.Ident => qctx.tasty.Term): qctx.tasty.Term = {
    import qctx.tasty.{_,given _}
    import scala.internal.quoted.showName
    import scala.quoted.QuoteContext
    import scala.quoted.Expr
    val expr = (rhs.seal: @unchecked) match {
      case '{ $rhsExpr: $t } =>
        '{
          @showName(${Expr(name)})
          val x = $rhsExpr
          ${
            val id = ('x).unseal.asInstanceOf[Ident]
            body(id).seal
          }
        }
    }
    expr.unseal
  }


