package cps.plugin.forest

import cps.plugin.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*

object AwaitTransform {

  def apply(term: Apply, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {

    val cpsTree = term match
      case Apply(Apply(TypeApply(fCpsAwaitCn, List(tf, ta, tg)), List(fa)), List(gc, gcn)) =>
        println(s" form at : ${term.show},  symbol=${fCpsAwaitCn.symbol}")
        if (fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.cpsAwait") ||
            fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.await")        ) then
          //def cpsAwait[F[_], A, G[_]](fa: F[A])(using CpsMonadContext[G], CpsMonadConversion[F, G]): A =
           fromApply(term, owner, nesting, tf, ta, tg, fa, gc, gcn)
        else
           throw CpsTransformException("Invalid term,  should be cpsAwait", term.srcPos)
      case _ =>
        throw CpsTransformException("Invalid term,  should be Apply(Apply(cpsAwait ... ))", term.srcPos)
    cpsTree
  }

  def fromApply(term: Apply, owner: Symbol, nesting:Int,
                fMonadType: Tree, aType: Tree, gMonadType:Tree,
                internalTerm: Tree,
                gMonadContext: Tree,
                gMonadConversion: Tree
               )(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(i"AwaitTransform.fromApply, internalTerm: ${internalTerm.show}", nesting)

    val internalCpsTree = RootTransform(internalTerm, owner, nesting+1)

    def convertToGMonad(internalTerm: Tree): Tree =
      if (fMonadType.tpe =:= gMonadType.tpe) then
        internalTerm
      else
        Apply(
          TypeApply(Select(gMonadConversion, "apply".toTermName),List(aType)),
          List(internalTerm)
        )

    val retval = internalCpsTree.asyncKind match
      case AsyncKind.Sync =>
          CpsTree.impure(term,owner,convertToGMonad(internalCpsTree.unpure.get), AsyncKind.Sync)
      case ik@AsyncKind.Async(ik1) =>
        val gInternalCpsTree = if (fMonadType.tpe =:= gMonadType.tpe) then {
          internalCpsTree
        } else {
          CpsTree.impure(term,owner,convertToGMonad(internalCpsTree.transformed), AsyncKind.Async(ik))
        }
        val monadRef = Select(gMonadContext,"monad".toTermName)
        val nTerm = Apply(TypeApply(Select(monadRef,"flatten".toTermName),List(aType)),List(gInternalCpsTree.transformed))
        CpsTree.impure(term, owner, nTerm, ik1)
      case AsyncKind.AsyncLambda(_) =>
        throw CpsTransformException("cpsAwait is not supported for async lambdas", term.srcPos)

    Log.trace(s"AwaitTransform.fromApply, retval: ${retval.show}", nesting)
    retval

  }

}
