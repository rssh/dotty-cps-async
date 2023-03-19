package cps.plugin

import scala.annotation.tailrec

import dotty.tools.dotc.*
import ast.tpd.*
import ast.{Trees,tpd}
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import util.SrcPos

import plugins.*

import cps.plugin.QuoteLikeAPI.*


enum DefDefSelectKind {
  case USING_CONTEXT_PARAM(cpsMonadContext: Tree)
  case RETURN_CONTEXT_FUN(internal: DefDefSelectKind)
}

case class DefDefSelectRecord(
                               val kind: DefDefSelectKind,
                               var internal: Boolean
                             )


class SelectedNodes {

  val allDefDefs: MutableSymbolMap[DefDefSelectRecord] = new MutableSymbolMap()

  def addDefDef(sym: Symbol, kind: DefDefSelectKind): Unit = {
    allDefDefs.update(sym, DefDefSelectRecord(kind,false))
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
    findCpsMonadContextParam(tree.paramss, tree.srcPos) match
      case Some(cpsMonadContext) => f(tree, cpsMonadContext)
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

  def findCpsMonadContextParam(value: List[Trees.ParamClause[Types.Type]], srcPos: SrcPos)(using Context): Option[Tree] = {
    findAllCpsMonadContextParam(value, List.empty) match
      case head :: Nil => Some(head)
      case head :: tail =>
        // later we can combine many contexts ar one using effect stacks or monad transformeds.
        throw CpsTransformException("Few monadcontexts in one function is not supported yet", srcPos)
      case Nil => None
  }

  @tailrec
  private def findAllCpsMonadContextParam(paramss: List[Trees.ParamClause[Types.Type]],
                                          acc: List[ValDef])(using Context): List[Tree] = {
    paramss match
      case paramssHead :: paramssTail =>
        paramssHead match
          case paramsHead :: paramTail =>
            paramsHead match
              case vd: ValDef =>
                val filtered = paramssHead.asInstanceOf[List[ValDef]].filter((p: ValDef) => CpsTransformHelper.isCpsMonadContextType(p.tpt.tpe))
                findAllCpsMonadContextParam(paramssTail, filtered ++ acc)
              case _ =>
                findAllCpsMonadContextParam(paramssTail, acc)
          case Nil =>
            findAllCpsMonadContextParam(paramssTail, acc)
      case Nil =>
        acc
  }


}