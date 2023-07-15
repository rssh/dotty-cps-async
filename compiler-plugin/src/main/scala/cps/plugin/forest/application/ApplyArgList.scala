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

sealed trait ApplyArgList {
  def isAsync(using Context, CpsTopLevelContext): Boolean
  def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def containsAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def containsDirectContext: Boolean
  def show(using Context):String
  def origin: Tree
  def isTypeParams: Boolean
  def isPlainParams: Boolean
}


case class ApplyTermArgList(
  originApplyTerm:  Apply,
  args: List[ApplyArg]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = args.exists(_.isAsync)
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = args.exists(_.isAsyncLambda)
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) =
    args.exists(x => x.isAsyncLambda && !x.lambdaCanBeUnshifted )
  override def containsDirectContext = args.exists(_.isDirectContext)
  override def origin = originApplyTerm
  override def isTypeParams = false
  override def isPlainParams = true

  override def show(using Context): String = {
    s"ApplyTermArgList(${args.map(_.show)})"
  }

}

case class ApplyTypeArgList(
  originApplyTerm:  TypeApply,
  args: List[TypeTree]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = false
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = false
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) = false
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

  def make(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): ApplyTermArgList = {
     val s0 = BuildState(List.empty,Set.empty,0)
     val s = term.args.foldLeft(s0){ (s,a) =>
        val (aExpr, named) = a match
          case NamedArg(name, expr) => (expr, Some(name.toTermName))
          case expr => (expr, None)
        val depResult = DependencyCheck.run(aExpr,s.symbols)
        val nApplyArg = ApplyArg( aExpr,
          mt.paramName(s.index, a.srcPos).toTermName,  
          mt.paramType(s.index, a.srcPos),
          mt.isByName(s.index, a.srcPos),
          mt.isDirectContext(s.index, a.srcPos),
          owner,
          depResult.canBeDependent,
          named,
          nesting
        )
        s.copy(revApplyArgs = nApplyArg::s.revApplyArgs, symbols = s.symbols ++ depResult.syms)
     }
     ApplyTermArgList(term, s.revApplyArgs.reverse)
  }

}
