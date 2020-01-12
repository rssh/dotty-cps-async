package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


object TransformUtil

  def skipInlined(given qctx:QuoteContext)(tree: qctx.tasty.Term):qctx.tasty.Term =
    import qctx.tasty.{_,given}
    tree match 
      case Inlined(origin, binding, expansion) => skipInlined(expansion)
      case _ => tree

  def makePure[F[_]:Type](given qctx:QuoteContext)(monad: Expr[AsyncMonad[F]], 
                                          tree: qctx.tasty.Term):qctx.tasty.Term =
    import qctx.tasty.{_,given}
    val selectExpr = '{ ${monad}.pure }
    Apply(
      selectExpr.unseal,List(tree))


  
  def find(given qctx:QuoteContext)(term: qctx.tasty.Term, 
                       cond: qctx.tasty.Tree=> Option[qctx.tasty.Tree]) :Option[qctx.tasty.Tree] = {
     import qctx.tasty.{_,given}
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(given ctx: Context): Option[Tree] =
                 foldOverTree(x,tree)

        override def foldOverTree(x: Option[Tree], tree: Tree)(given ctx: Context): Option[Tree] = {
           if (x.isDefined) 
             x
           else 
             println(s"search, tree=${tree}")
             cond(tree) orElse super.foldOverTree(x,tree)
        }
     }
     search.foldTree(None,term)
  }

 
  /**
   * substitute identifier with the origin symbol to new tree
   **/
  def substituteIdent(given qctx:QuoteContext)(tree: qctx.tasty.Term, 
                           origin: qctx.tasty.Symbol, 
                           newTerm: qctx.tasty.Term): qctx.tasty.Term =
     import qctx.tasty.{_,given}
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(given ctx: Context):Term =
          tree match 
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newTerm
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


