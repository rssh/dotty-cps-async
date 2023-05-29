package cps.plugin.forest


import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*


object WhileTransform {

  def apply(term: WhileDo, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val cpsCond = RootTransform(term.cond, owner, nesting+1)
    val cpsBody = RootTransform(term.body, owner, nesting+1)
    val tctx = summon[CpsTopLevelContext]
    val retval =  (cpsCond.asyncKind, cpsBody.asyncKind) match
      case (AsyncKind.Sync, AsyncKind.Sync) =>
        if (cpsCond.isOriginEqSync && cpsBody.isOriginEqSync) {
          CpsTree.unchangedPure(term,owner)
        } else {
          CpsTree.pure(term, owner, WhileDo(cpsCond.unpure.get, cpsBody.unpure.get).withSpan(term.span))
        }
      case (AsyncKind.Sync, AsyncKind.Async(ik)) =>
        val helperMethod = Symbols.requiredMethod("cps.runtime.WhileHelper.w01")
        val nTree = Apply(
          TypeApply(ref(helperMethod),List(TypeTree(tctx.monadType))),
          List(tctx.cpsMonadRef,term.cond,cpsBody.transformed)
        )
        CpsTree.impure(term,owner,nTree,ik)
      case (AsyncKind.Async(ik), AsyncKind.Sync) =>
        val helperMethod = Symbols.requiredMethod("cps.runtime.WhileHelper.w10")
        val nTree = Apply(
          TypeApply(ref(helperMethod), List(TypeTree(tctx.monadType))),
          List(tctx.cpsMonadRef, cpsCond.transformed, term.body)
        )
        CpsTree.impure(term,owner,nTree,ik)
      case (AsyncKind.Async(ik1), AsyncKind.Async(ik2)) =>
        if (ik1 != ik2) then
          throw CpsTransformException("Different async kinds inwhile components", term.srcPos)
        val helperMethod = Symbols.requiredMethod("cps.runtime.WhileHelper.w11")
        val nTree = Apply(
          TypeApply(ref(helperMethod), List(TypeTree(tctx.monadType))),
          List(tctx.cpsMonadRef, cpsCond.transformed, cpsBody.transformed)
        )
        CpsTree.impure(term,owner,nTree,ik1)
      case _ =>
        throw CpsTransformException("Impossible: one of while components is asyncLambda", term.srcPos)
    retval
  }

}
