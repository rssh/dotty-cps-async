package cps.plugin.forest.application

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*

import cps.plugin.*
import cps.plugin.forest.*


sealed trait ApplyArg {
    def name: TermName
    def tpe:  Type

    def isAsync: Boolean
}

object ApplyArg {

  def apply(expr: Tree, index: Int, paramName: TermName, paramType: Type, isByName: Boolean, owner: Symbol, tctx: TransformationContext)(using Context): ApplyArg = {
    expr match
      case SeqLiteral(elems, elementtp) =>
        RepeatApplyArg(paramName, paramType, elems.map(p => RootTransform(p,owner,tctx) ))
      case _ =>
        val cpsExpr = RootTransform(expr, owner, tctx)
        if (isByName) then
          ByNameApplyArg(paramName, paramType, cpsExpr)
        else
          paramType match
            case AnnotatedType(tp, an) if an.symbol == defn.InlineParamAnnot =>
                InlineApplyArg(paramName,tp,cpsExpr)
            case AnnotatedType(tp, an) if an.symbol == defn.ErasedParamAnnot =>
                ErasedApplyArg(paramName,tp,cpsExpr)
            case _ =>
                PlainApplyArg(paramName,paramType,cpsExpr)    
  }

}

sealed trait ExprApplyArg extends ApplyArg {

  def expr: CpsTree

  override def isAsync = expr.isAsync

}

case class PlainApplyArg(  
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree,  
) extends ExprApplyArg

case class RepeatApplyArg(
  override val name: TermName,
  override val tpe: Type,
  exprs: List[CpsTree]
) extends ApplyArg {

  override def isAsync = exprs.exists(_.isAsync)

}

case class ByNameApplyArg(
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree
) extends ExprApplyArg

// potentially not used if we run after macros,
// but we can return 
case class InlineApplyArg(
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree
) extends ExprApplyArg

case class ErasedApplyArg(
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree
) extends ExprApplyArg
