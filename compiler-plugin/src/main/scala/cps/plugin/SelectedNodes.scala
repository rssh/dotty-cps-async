package cps.plugin

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import ast.{Trees, tpd}
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import cps.plugin
import util.SrcPos
import plugins.*
import cps.plugin.QuoteLikeAPI.*


enum DefDefSelectKind {

  case USING_CONTEXT_PARAM(val cpsDirectContext: Tree)
  case RETURN_CONTEXT_FUN(internal: DefDefSelectKind)

  def getCpsDirectContext: Tree = this match
    case USING_CONTEXT_PARAM(cmc) => cmc
    case RETURN_CONTEXT_FUN(internal) => internal.getCpsDirectContext

}

/**
 * Record for defDef, which was selected for transformation.
 * Note, that this shouln not be case class, because it used in MutableSymbolMap
 * and it is important to have equals/hashCode methods not depended from value (i.e. - system).
 * @param kind
 * @param internal
 */
class DefDefSelectRecord(val kind: DefDefSelectKind,
                         var internal: Boolean,
                         var monadType: Type = NoType,
                         var changedType: Type = NoType,
                         var debugLevel: Int = 0
                        )


class SelectedNodes {

  val allDefDefs: MutableSymbolMap[DefDefSelectRecord] = new MutableSymbolMap()

  def addDefDef(sym: Symbol, kind: DefDefSelectKind): Unit = {
    allDefDefs.get(sym) match
      case Some(r) =>
        throw IllegalStateException(s"defDef already exists: ${sym}")
      case None =>
        allDefDefs.update(sym, DefDefSelectRecord(kind,false))
  }

  def markAsInternal(sym: Symbol): Unit = {
    allDefDefs.get(sym) match
      case Some(r) =>
        r.internal= true
      case None =>
        throw IllegalStateException(s"defDef not found in markIntenal: ${sym}")
  }

  def defDefKindIfTopLevel(sym:Symbol): Option[DefDefSelectKind] = {
    allDefDefs.get(sym).flatMap(r =>
      Option.when(!r.internal)(r.kind)
    )
  }

  def getDefDefRecord(sym:Symbol): Option[DefDefSelectRecord] =
    allDefDefs.get(sym)

  def isTopLevel(sym: Symbol): Boolean = {
    allDefDefs.get(sym).exists(!_.internal)
  }

  
  
}


object SelectedNodes {


  /**
   * determinate kind
   */
  def detectDefDefSelectKind(tree: DefDef)(using Context): Option[DefDefSelectKind] = {
    val optKind = SelectedNodes.checkAndProcessDefDef(tree) {
      (tree, monadContext) => Some(DefDefSelectKind.USING_CONTEXT_PARAM(monadContext))
    } {
      (tree, kind) => Some(DefDefSelectKind.RETURN_CONTEXT_FUN(kind))
    }
    optKind
  }

  /**
   *
   * @param tree: tree to process
   * @param f: (defDef(selected function),  Tree: (CpsMonadContext parameter)) => Option[A] is called
   *    when we find parameter with type CpsMonadContext.
   * @param acc
   * @param Context
   * @tparam A
   * @return
   */
  def checkAndProcessDefDef[A](tree:DefDef)(f: (DefDef, Tree) => Option[A])(acc:(DefDef,A)=>Option[A])(using Context):Option[A] = {
    findCpsDirectContextParam(tree.paramss, tree.srcPos) match
      case Some(cpsDirectContext) => f(tree, cpsDirectContext)
      case None =>
        //  TODO: are we transform only contect functions [?]. mb.all?
        if (Symbols.defn.isContextFunctionType(tree.tpt.tpe)) then
          tree.rhs match
            case Block((ddef: DefDef) :: Nil, closure: Closure)  if ddef.symbol == closure.meth.symbol =>
              // ContextLambda
              checkAndProcessDefDef(ddef)(f)(acc) match
                case Some(a) =>
                  acc(tree,a)
                case None =>
                  None
            case _ => None
        else
          None
  }

  def findCpsDirectContextParam(value: List[Trees.ParamClause[Types.Type]], srcPos: SrcPos)(using Context): Option[Tree] = {
    findAllCpsDirectContextParam(value, List.empty) match
      case head :: Nil => Some(head)
      case head :: tail =>
        // later we can combine many contexts ar one using effect stacks or monad transformeds.
        throw CpsTransformException("Few monadcontexts in one function is not supported yet", srcPos)
      case Nil => None
  }



  @tailrec
  private def findAllCpsDirectContextParam(paramss: List[Trees.ParamClause[Types.Type]],
                                          acc: List[ValDef])(using Context): List[Tree] = {
    paramss match
      case paramssHead :: paramssTail =>
        paramssHead match
          case paramsHead :: paramTail =>
            paramsHead match
              case vd: Trees.ValDef[?] =>
                val filtered = paramssHead.asInstanceOf[List[ValDef]].filter((p: ValDef) => CpsTransformHelper.isCpsDirectType(p.tpt.tpe))
                findAllCpsDirectContextParam(paramssTail, filtered ++ acc)
              case _ =>
                findAllCpsDirectContextParam(paramssTail, acc)
          case Nil =>
            findAllCpsDirectContextParam(paramssTail, acc)
      case Nil =>
        acc
  }


}