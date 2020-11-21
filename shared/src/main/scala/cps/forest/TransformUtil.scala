package cps.forest

import scala.quoted._

import cps._
import cps.misc._


object TransformUtil:


  def find(using qctx:QuoteContext)(term: qctx.reflect.Term,
                       cond: qctx.reflect.Tree=> Option[qctx.reflect.Tree]) :Option[qctx.reflect.Tree] = {
     import qctx.reflect._
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] =
                 foldOverTree(x,tree)(owner)

        override def foldOverTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse super.foldOverTree(x,tree)(owner)
        }
     }
     search.foldTree(None,term)(Symbol.spliceOwner)
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
        override def transformTerm(tree:Term)(owner: Symbol):Term =
          tree match
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newTerm
                                      } else {
                                         super.transformTerm(tree)(owner)
                                      }
            case _ => super.transformTerm(tree)(owner)
     }
     changes.transformTerm(tree)(Symbol.spliceOwner)


  def createFunctionType(using qctx: QuoteContext)(argTypes: List[qctx.reflect.TypeRepr], 
                                                   resultType: qctx.reflect.TypeRepr): qctx.reflect.TypeRepr =
    import qctx.reflect._
    val funSymbol = defn.FunctionClass(argTypes.size)
    val funTypeTree: TypeTree = TypeIdent(funSymbol)
    funTypeTree.tpe.appliedTo(argTypes.map(_.widen) :+ resultType.widen)


