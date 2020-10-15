package cps.forest

import scala.quoted._

import cps._


object TransformUtil:


  def find(using qctx:QuoteContext)(term: qctx.reflect.Term,
                       cond: qctx.reflect.Tree=> Option[qctx.reflect.Tree]) :Option[qctx.reflect.Tree] = {
     import qctx.reflect._
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        //def foldTree(x: Option[Tree], tree: Tree)(using Owner): Option[Tree] =
        def foldTree(x: Option[Tree], tree: Tree)(using ctx:Context): Option[Tree] =
                 foldOverTree(x,tree)

        //override def foldOverTree(x: Option[Tree], tree: Tree)(using Owner): Option[Tree] = {
        override def foldOverTree(x: Option[Tree], tree: Tree)(using Context): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse super.foldOverTree(x,tree)
        }
     }
     search.foldTree(None,term)
  }

  def containsAwait(using qctx:QuoteContext)(term: qctx.reflect.Term): Boolean =
    import qctx.reflect._
    find(term, {
           case v@Apply(TypeApply(id@Ident("await"),targs),args) =>
                         if (id.symbol.fullName == "cps.await") Some(v) else None
           case _ => None
         }).isDefined


  /**
   * substitute identifier with the origin symbol to new tree
   **/
  def substituteIdent(using qctx:QuoteContext)(tree: qctx.reflect.Term,
                           origin: qctx.reflect.Symbol,
                           newTerm: qctx.reflect.Term): qctx.reflect.Term =
     import qctx.reflect._
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(using Context):Term =
          tree match
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newTerm
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


  def namedLet(using qctx: QuoteContext)(name: String, rhs: qctx.reflect.Term)(body: qctx.reflect.Ident => qctx.reflect.Term): qctx.reflect.Term = {
    import qctx.reflect._
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


