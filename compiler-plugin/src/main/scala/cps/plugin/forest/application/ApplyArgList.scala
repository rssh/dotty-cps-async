package cps.plugin.forest.application

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*
import cps.plugin.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.tpd

sealed trait ApplyArgList {
  def isAsync(using Context, CpsTopLevelContext): Boolean
  def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def containsAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def findDirectContext: Option[Tree]
  def containsDirectContext: Boolean
  def show(using Context): String
  def origin: Tree
  def isTypeParams: Boolean
  def isPlainParams: Boolean
}

case class ApplyTermArgList(
    originApplyTerm: Apply,
    args: List[ApplyArg]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = args.exists(_.isAsync)
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = args.exists(_.isAsyncLambda)
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) =
    args.exists(x => x.isAsyncLambda && !x.lambdaCanBeUnshifted)
  override def findDirectContext: Option[Tree] = args.find(_.isDirectContext).map(_.origin)
  override def containsDirectContext = args.exists(_.isDirectContext)
  override def origin = originApplyTerm
  override def isTypeParams = false
  override def isPlainParams = true

  override def show(using Context): String = {
    s"ApplyTermArgList(${args.map(_.show)})"
  }

}

case class ApplyTypeArgList(
    originApplyTerm: TypeApply,
    args: List[TypeTree]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = false
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = false
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) = false
  override def findDirectContext: Option[Tree] = None
  override def containsDirectContext = false
  override def origin = originApplyTerm
  override def show(using Context): String = {
    s"ApplyTypeArgList(${args})"
  }
  override def isTypeParams = true
  override def isPlainParams = false
}

object ApplyTermArgList {

  case class BuildState(
      revApplyArgs: List[ApplyArg],
      symbols: Set[Symbol],
      index: Int
  )

  def make(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using
      Context,
      CpsTopLevelContext
  ): ApplyTermArgList = {
    val s0 = BuildState(List.empty, Set.empty, 0)
    val s = term.args.foldLeft(s0) { (s, a) =>
      val (aExpr, named) = a match
        case NamedArg(name, expr) => (expr, Some(name.toTermName))
        case expr                 => (expr, None)
      val isDirectContext = mt.isDirectContext(s.index, a.srcPos)
      val depResult = if (isDirectContext) {
        //  direct context is always syntetic and substituted by real context in the code
        //  (if we will generate temporary val for it, we broke this substitution)
        //  (mb add contextual argument for byIdentityCall to set).
        DependencyCheck.Result(false, Set.empty)
      } else {
        DependencyCheck.run(aExpr, s.symbols)
      }
      val nApplyArg = ApplyArg(
        aExpr,
        mt.paramName(s.index, a.srcPos).toTermName,
        mt.paramType(s.index, a.srcPos),
        mt.isByName(s.index, a.srcPos),
        isDirectContext,
        owner,
        depResult.canBeDependent,
        named,
        nesting
      )
      s.copy(revApplyArgs = nApplyArg :: s.revApplyArgs, symbols = s.symbols ++ depResult.syms, index = s.index + 1)
    }
    ApplyTermArgList(term, s.revApplyArgs.reverse)
  }

}
