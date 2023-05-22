package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*
import cps.plugin.*
import cps.plugin.forest.cases.*


object TryTransform {

  def apply(tryTerm:Try, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    val cpsExpr = RootTransform(tryTerm.expr, owner, nesting + 1)
    if (tryTerm.cases.isEmpty && tryTerm.finalizer.isEmpty) {
      // no cases, no catch = just expr
      cpsExpr
    } else if (tryTerm.cases.isEmpty && !tryTerm.finalizer.isEmpty) {
      val cpsFinalizer = RootTransform(tryTerm.finalizer, owner, nesting + 1)
      applyNoCases(tryTerm, owner, nesting, cpsExpr, cpsFinalizer)
    } else if (!tryTerm.cases.isEmpty && tryTerm.finalizer.isEmpty) {
      val cpsCases = CpsCases.create(tryTerm.cases, owner, nesting + 1)
      //applyNoFinalizer(tryTerm, owner, nesting, cpsExpr, cpsCases)
      ???
    } else {
      val cpsCases = CpsCases.create(tryTerm.cases, owner, nesting + 1)
      val cpsFinalizer = RootTransform(tryTerm.finalizer, owner, nesting + 1)
      //applyFull(tryTerm, owner, nesting, cpsExpr, cpsCases, cpsFinalizer)
      ???
    }
  }

  def applyNoCases(tryTerm:Try, owner: Symbol, nesting:Int, cpsExpr: CpsTree, cpsFinalizer: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    (cpsExpr.asyncKind, cpsFinalizer.asyncKind) match {
      case (AsyncKind.Sync, AsyncKind.Sync) =>
         if (cpsExpr.isOriginEqSync && cpsFinalizer.isOriginEqSync)
            CpsTree.unchangedPure(tryTerm, owner)
         else
            val newTree = Try(cpsExpr.unpure.get, List.empty, cpsFinalizer.unpure.get)
            CpsTree.pure(tryTerm, owner, newTree)
      case (AsyncKind.Sync, AsyncKind.Async(ik)) =>
          generateWithAsyncFinalizerLazy(tryTerm, owner, cpsExpr, cpsFinalizer)
      case (_,AsyncKind.AsyncLambda(bodyKind)) =>
          throw CpsTransformException(s"try with async-lambda as finalizer is not supported for ${cpsExpr}", tryTerm.srcPos)
      case _ =>
          generateWithAsyncFinalizer(tryTerm, owner, cpsExpr, cpsFinalizer)
    }
  }

  private def generateWithAsyncFinalizerLazy(
                                          origin: Try,
                                          owner: Symbol,
                                          matchCpsTree: CpsTree,
                                          finalizerCpsTree: CpsTree,
                                        )(using Context, CpsTopLevelContext): CpsTree = {
    ???
  }

  private def generateWithAsyncFinalizer(
                                          origin: Try,
                                          owner: Symbol,
                                          matchCpsTree: CpsTree,
                                          finalizerCpsTree: CpsTree,
                                          )(using Context, CpsTopLevelContext): CpsTree = {
    val tctx = summon[CpsTopLevelContext]
    val trySupport = tctx.optTrySupport.getOrElse(
      throw CpsTransformException(s"try support is not enabled for ${tctx.monadType}", origin.srcPos)
    )
    val tree = Apply(
      Apply(
        TypeApply(
          Select(trySupport, "withAsyncFinalizer".toTermName),
          List(TypeTree(matchCpsTree.originType.widen))
        ),
        List(matchCpsTree.transformed)
      ),
      List(finalizerCpsTree.transformed)
    ).withSpan(origin.span)
    matchCpsTree.asyncKind match
      case AsyncKind.Sync => AsyncKind.Sync
         CpsTree.impure(origin,owner,tree,AsyncKind.Sync)
      case AsyncKind.Async(ik) =>
          CpsTree.impure(origin,owner,tree,ik)
      case AsyncKind.AsyncLambda(bodyKind) =>
          CpsTree.opaqueAsyncLambda(origin,owner,tree,bodyKind)

  }



}
