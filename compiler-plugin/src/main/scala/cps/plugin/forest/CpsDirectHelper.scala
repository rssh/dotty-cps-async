package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.{Symbols, *}
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.StdNames.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span
import core.Types.*
import core.Phases.*
import cps.plugin.*
import cps.{CpsMonadContext, CpsMonadConversion}
import inlines.Inlines
import transform.Inlining


object CpsDirectHelper {

  /**
   * generate new CpsDirect[tf](fctx)  (positioned at posTerm)
   * @param tf - type of wrapper monad 
   * @param fctx - current monnad context
   * @param posTerm - position of new term
   * @return new CpsDirect[tf](fctx)
   */
  def genCpsDirectDefaultConstructor(tf: TypeTree, fctx: Tree, posSpan: Span)(using Context): Tree =
    val cpsDirectType = Symbols.requiredClassRef("cps.CpsDirect").appliedTo(tf.tpe)
    val cpsDirectConstructor = Select(New(TypeTree(cpsDirectType)), "<init>".toTermName)
    Apply(
      TypeApply(cpsDirectConstructor, List(tf)),
      List(fctx)
    ).withSpan(posSpan)


  /**
   * generate fgincl.apply(fctx)(lambda)
   */
  def genConventionCall(fctx:Tree, fgincl: Tree, originType:Type, lambda: Tree, span: Span)(using Context): Tree = {
    //trait CpsMonadContextInclusion[F[_],G[_]] {
    //
    //  def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[G] => G[T]):F[T]
    //
    //}
    Apply(
      Apply(
        TypeApply(Select(fgincl, "apply".toTermName), List(TypeTree(originType.widen))),
        List(fctx)
      ),
      List(lambda)
    ).withSpan(span)

  }

  
  def substituteCpsDirectArgInCall(applyOrTypeApply: Tree, originCPsDirectArg: ByInclusionCallArgs, nCpsDirectArg: Tree)(using Context): Option[Tree] = {
    applyOrTypeApply match
      case app: Apply =>
        CpsDirectHelper.substituteCpsDirectArgInApply(app, originCPsDirectArg, nCpsDirectArg)
      case ta@TypeApply(fn, targs) =>
        substituteCpsDirectArgInCall(fn, originCPsDirectArg, nCpsDirectArg).map(x => TypeApply(x, targs).withSpan(ta.span))
      case other =>
        None
  }

  def substituteCpsDirectArgInApply(applyTerm: Apply, originCPsDirectArg: ByInclusionCallArgs, nCpsDirectArg: Tree)(using Context): Option[Apply] = {
    var changed = false
    val newArgs = applyTerm.args.map {
        case ByInclusionCall(tf,tg,fctx,fginc) =>
            if (!changed) then
              changed = true
              nCpsDirectArg
            else
              throw CpsTransformException("More than one CpsDirect argument in call", applyTerm.srcPos)
        case arg =>
          arg
    }
    if (changed) then
      Some(Apply(applyTerm.fun, newArgs).withSpan(applyTerm.span))
    else
      substituteCpsDirectArgInCall(applyTerm.fun, originCPsDirectArg, nCpsDirectArg).map{
        newFun => Apply(newFun, newArgs).withSpan(applyTerm.span)
      }
  }
  
  case class ByInclusionCallArgs(tf: Tree, tg: Tree, fctx: Tree, fginc: Tree)

  object ByInclusionCall {
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree, Tree, Tree)] =
      tree match
        case Apply(TypeApply(fn, List(tf, tg)), List(fctx, fgincl))
          if (fn.symbol == Symbols.requiredMethod("cps.CpsDirect.byInclusion")) =>
            Some((tf, tg, fctx, fgincl))
        case _ =>
            None
  }

}
