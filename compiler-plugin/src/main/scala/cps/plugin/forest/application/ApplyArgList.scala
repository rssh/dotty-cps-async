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
  def isAsync: Boolean
  def containsAsyncLambda: Boolean
  def show(using Context):String
}


case class ApplyTermArgList(
  originApplyTerm:  Apply,
  args: List[ApplyArg]
) extends ApplyArgList {
  override def isAsync = args.exists(_.isAsync)
  override def containsAsyncLambda = args.exists(_.isLambda)

  override def show(using Context): String = {
    s"ApplyTermArgList(${args.map(_.show)})"
  }

} 

case class ApplyTypeArgList(
  originApplyTerm:  TypeApply,
  args: List[TypeTree]
) extends ApplyArgList {
  override def isAsync = false
  override def containsAsyncLambda = false
  override def show(using Context): String = {
    s"ApplyTypeArgList(${args})"
  }
}


object ApplyTermArgList {

  case class BuildState(
    revApplyArgs: List[ApplyArg],
    symbols: Set[Symbol],
    index: Int
  )

  def make(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, tctx: TransformationContext)(using Context): ApplyTermArgList = {
     val s0 = BuildState(List.empty,Set.empty,0)
     val s = term.args.foldLeft(s0){ (s,a) =>
        val depResult = DependencyCheck.run(a,s.symbols)
        val nApplyArg = ApplyArg( a,
          mt.paramName(s.index, a.srcPos).toTermName,  
          mt.paramType(s.index, a.srcPos),
          mt.isByName(s.index, a.srcPos),
          owner,
          depResult.canBeDependent,
          tctx
        )
        s.copy(revApplyArgs = nApplyArg::s.revApplyArgs, symbols = s.symbols ++ depResult.syms)
     }
     ApplyTermArgList(term, s.revApplyArgs.reverse)
  }

}
