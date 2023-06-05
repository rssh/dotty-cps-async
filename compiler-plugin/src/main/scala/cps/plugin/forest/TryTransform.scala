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

import scala.collection.immutable.List
import scala.util.control.NonFatal


object TryTransform {

  def apply(tryTerm:Try, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"TryTransform, origin=${tryTerm}", nesting)
    val cpsExpr = RootTransform(tryTerm.expr, owner, nesting + 1)
    val retval = if (tryTerm.cases.isEmpty && tryTerm.finalizer.isEmpty) {
      // no cases, no catch = just expr
      cpsExpr
    } else if (tryTerm.cases.isEmpty && !tryTerm.finalizer.isEmpty) {
      val cpsFinalizer = RootTransform(tryTerm.finalizer, owner, nesting + 1)
      applyNoCases(tryTerm, owner, nesting, cpsExpr, cpsFinalizer)
    } else if (!tryTerm.cases.isEmpty && tryTerm.finalizer.isEmpty) {
      val cpsCases = CpsCases.create(tryTerm.cases, owner, nesting + 1)
      applyNoFinalizer(tryTerm, owner, nesting, cpsExpr, cpsCases)
    } else {
      val cpsCases = CpsCases.create(tryTerm.cases, owner, nesting + 1)
      val cpsFinalizer = RootTransform(tryTerm.finalizer, owner, nesting + 1)
      applyFull(tryTerm, owner, nesting, cpsExpr, cpsCases, cpsFinalizer)
    }
    Log.trace(s"TryTransform, retval=${retval.show}", nesting)
    retval
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
          generateWithAsyncFinalizerInTry(tryTerm, owner, cpsExpr, cpsFinalizer)
      case (_,AsyncKind.AsyncLambda(bodyKind)) =>
          throw CpsTransformException(s"try with async-lambda as finalizer is not supported for ${cpsExpr}", tryTerm.srcPos)
      case _ =>
          generateWithAsyncFinalizer(tryTerm, owner, cpsExpr, cpsFinalizer)
    }
  }

  def applyNoFinalizer(origin: Try, owner: Symbol, nesting: Int, cpsExpr: CpsTree, cases: CpsCases)(using Context, CpsTopLevelContext): CpsTree = {
    val casesAsyncKind = cases.collectAsyncKind
    (cpsExpr.asyncKind, casesAsyncKind) match {
      case (AsyncKind.Sync, AsyncKind.Sync) =>
         if (cpsExpr.isOriginEqSync && cases.unchanged)
            CpsTree.unchangedPure(origin, owner)
         else
            val newTree = Try(cpsExpr.unpure.get, cases.unpureCaseDefs, EmptyTree)
            CpsTree.pure(origin, owner, newTree)
      case (AsyncKind.Sync, AsyncKind.Async(ik)) =>
          val retval = generateWithAsyncCasesWithTry(origin, owner, cpsExpr, cases, casesAsyncKind)
          Log.trace(s"TryTransform:applyNoFinalizer return ${retval.show}", nesting)
          retval
      case _ =>
          val targetKind = cpsExpr.asyncKind unify casesAsyncKind match
            case Left((k1,k2)) =>
              throw CpsTransformException("Incompatible async kinds of try expr and cases", origin.srcPos)
            case Right(k) => k
          generateWithAsyncCases(origin, owner, cpsExpr, cases, targetKind)
    }
  }

  def applyFull(origin: Try, owner: Symbol, nesting: Int, cpsExpr: CpsTree, cases: CpsCases, cpsFinalizer: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    val casesAsyncKind = cases.collectAsyncKind
    (cpsExpr.asyncKind, casesAsyncKind, cpsFinalizer.asyncKind) match
      case (AsyncKind.Sync, AsyncKind.Sync, AsyncKind.Sync) =>
        if (cpsExpr.isOriginEqSync && cases.unchanged && cpsFinalizer.isOriginEqSync) {
          CpsTree.unchangedPure(origin, owner)
        } else {
          val nTry = Try(cpsExpr.unpure.get, cases.unpureCaseDefs, cpsFinalizer.unpure.get).withSpan(origin.span)
          CpsTree.pure(origin, owner, nTry)
        }
      case (_, _, AsyncKind.AsyncLambda(_)) =>
        throw CpsTransformException("Try finalizer can't be an async lambda", cpsFinalizer.origin.srcPos)
      case (AsyncKind.Sync, AsyncKind.Sync, _) =>
        val syncTry = Try(cpsExpr.unpure.get, cases.unpureCaseDefs, EmptyTree).withSpan(origin.span)
        //val pureSyncTry = Apply(
        //  TypeApply(
        //    Select(summon[CpsTopLevelContext].cpsMonadRef, "pure".toTermName)
        //  )
        //)
        val pureSyncTry = CpsTree.pure(origin, owner, syncTry)
        val wrapped = wrapPureCpsTreeInTry(origin, pureSyncTry)
        val wrappedExpr = CpsTree.impure(origin, owner, wrapped, AsyncKind.Sync)
        generateWithAsyncFinalizer(origin, owner, wrappedExpr, cpsFinalizer)
      case (AsyncKind.AsyncLambda(il1), AsyncKind.AsyncLambda(il2), finalizerKind) =>
        if (il1 == il2) {
          finalizerKind match
            case AsyncKind.Sync =>
              val nCases = cases.transformedCaseDefs(cpsExpr.asyncKind, origin.tpe.widen)
              println("TryTransform::applyFull:(AsyncLambda,AsyncLambda,Sync)")
              println(s"cases.cases.head.cpsBody = ${cases.cases.head.cpsBody.show}")
              println(s"cases.cases.head.cpsBody.transformed = ${cases.cases.head.cpsBody.transformed.show}")
              println(s"caess.cases.head.cpsBody.cast(${origin.tpe.widen.show}) = ${cases.cases.head.cpsBody.castOriginType(origin.tpe.widen).show}")
              val nTry = Try(cpsExpr.transformed, nCases, cpsFinalizer.unpure.get)
              CpsTree.opaqueAsyncLambda(origin, owner, nTry,il1)
            case AsyncKind.Async(fk) =>
              val nTry = Try(cpsExpr.transformed, cases.transformedCaseDefs(cpsExpr.asyncKind, origin.tpe.widen), EmptyTree)
              val expr = CpsTree.opaqueAsyncLambda(origin, owner, nTry,il1)
              generateWithAsyncFinalizerTree(origin, owner, cpsExpr.transformed, cpsExpr.transformedType,expr.asyncKind,cpsFinalizer)
            case AsyncKind.AsyncLambda(x) =>
              // impossible, but make compiler happy
              throw CpsTransformException("Try finalizer can't be an async lambda", cpsFinalizer.origin.srcPos)
        } else {
          throw CpsTransformException(s"Non-cpompatible async shape in try extression and handlers", origin.srcPos)
        }
      case (exprKind, casesKind, _) =>
        exprKind.unify(casesKind) match
          case Left(p) =>
            println(s"expr=${cpsExpr.show},  expr.kind=${cpsExpr.asyncKind} ")
            println(s"cases = ${cases.unpureCaseDefs.map(_.show)}  cases.kind=${casesAsyncKind} ")
            throw CpsTransformException(s"Non-compatible async shape in try exppression and handlers ${p}", origin.srcPos)
          case Right(k) =>
            val expr1 = generateWithAsyncCases(origin, owner, cpsExpr, cases, k)
            val expr2 = generateWithAsyncFinalizer(origin, owner, expr1, cpsFinalizer)
            expr2
  }


  private def generateWithAsyncFinalizerInTry(
                                          origin: Try,
                                          owner: Symbol,
                                          exprCpsTree: CpsTree,
                                          finalizerCpsTree: CpsTree,
                                        )(using Context, CpsTopLevelContext): CpsTree = {
     generateWithAsyncFinalizerTree( origin, owner,
       wrapPureCpsTreeInTry(origin, exprCpsTree),
       exprCpsTree.originType.widen,
       exprCpsTree.asyncKind,
       finalizerCpsTree
     )
  }

  private def generateWithAsyncFinalizer(
                                          origin: Try,
                                          owner: Symbol,
                                          matchCpsTree: CpsTree,
                                          finalizerCpsTree: CpsTree,
                                        )(using Context, CpsTopLevelContext): CpsTree = {
    generateWithAsyncFinalizerTree(origin,owner,matchCpsTree.transformed,
      matchCpsTree.originType.widen, matchCpsTree.asyncKind,
      finalizerCpsTree)
  }

  private def generateWithAsyncFinalizerTree(
                                          origin: Try,
                                          owner: Symbol,
                                          arg: Tree,
                                          returnType: Type,
                                          argAsyncKind: AsyncKind,
                                          finalizerCpsTree: CpsTree,
                                          )(using Context, CpsTopLevelContext): CpsTree = {
    val tree = Apply(
      Apply(
        TypeApply(
          Select(trySupport(origin), "withAsyncFinalizer".toTermName),
          List(TypeTree(returnType.widen))
        ),
        List(arg)
      ),
      List(finalizerCpsTree.transformed)
    ).withSpan(origin.span)
    argAsyncKind match
      case AsyncKind.Sync =>
         CpsTree.impure(origin,owner,tree,AsyncKind.Sync)
      case AsyncKind.Async(ik) =>
          CpsTree.impure(origin,owner,tree,ik)
      case AsyncKind.AsyncLambda(bodyKind) =>
          CpsTree.opaqueAsyncLambda(origin,owner,tree,bodyKind)
  }

  private def generateWithAsyncCases(origin: Try, owner: Symbol, cpsExpr: CpsTree, cases: CpsCases, targetKind: AsyncKind)(using Context, CpsTopLevelContext): CpsTree = {
     println(s"Try: generateWithAsyncCases, targetKind=${targetKind}, cpsExpr=${cpsExpr.show}")
     val retval = generateExprWithAsyncErrorHandler(origin,owner,cpsExpr.transformed, origin.tpe.widen,  cases,targetKind)
     retval
  }

  private def generateExprWithAsyncErrorHandler(origin: Try, owner: Symbol, expr: Tree, exprUnwrappedType: Type, cases: CpsCases, targetKind: AsyncKind)(using Context, CpsTopLevelContext): CpsTree = {
    //
    //val sym = newSymbol(owner, "tryCases".toTermName, Flags.EmptyFlags)
    val exCasesType = targetKind match
      case AsyncKind.Sync => origin.tpe.widen
      case _ => CpsTransformHelper.cpsTransformedType(origin.tpe.widen, summon[CpsTopLevelContext].monadType)
    val transformedCases = cases.transformedCaseDefs(targetKind, origin.tpe.widen)
    val lambdaResultType = transformedCases.head.body.tpe.widen
    val mt = MethodType(List("ex".toTermName), List(defn.ThrowableType), lambdaResultType)
    val lambdaSym = newAnonFun(owner,mt)
    val lambda = Closure(lambdaSym, tss => {
         Match(tss.head.head, transformedCases).changeOwner(owner,lambdaSym)
       }
    )
    val tree = Apply(
      Apply(
        TypeApply(
          Select(trySupport(origin), "withAsyncErrorHandler".toTermName),
          List(TypeTree(origin.tpe.widen))
        ),
        List(expr)
      ),
      List(lambda)
    ).withSpan(origin.span)
    targetKind match
      case AsyncKind.Sync =>
         CpsTree.impure(origin,owner,tree,AsyncKind.Sync)
      case AsyncKind.Async(ik) =>
          CpsTree.impure(origin,owner,tree,ik)
      case AsyncKind.AsyncLambda(bodyKind) =>
          CpsTree.opaqueAsyncLambda(origin,owner,tree,bodyKind)
  }

  private def generateWithAsyncCasesWithTry(origin: Try, owner: Symbol, expr: CpsTree, cases: CpsCases, kind: AsyncKind)(using Context, CpsTopLevelContext): CpsTree = {
    generateExprWithAsyncErrorHandler(origin,owner,wrapPureCpsTreeInTry(origin,expr), expr.originType,  cases,kind)
  }

  private def wrapPureExprTreeInTry(origin: Try, expr: Tree, exprUnwrappedType:Type)(using Context, CpsTopLevelContext): Tree = {
        val bindSym = Symbols.newPatternBoundSymbol("ex".toTermName, defn.ThrowableType, origin.span)
        val nonFatalUnapplySym = Symbols.requiredMethod("scala.util.control.NonFatal.unapply")
        // TODO:  with deprecation of NonFatal think about cactu Exception (i.e. Typed instead Unaopplu)
        val nonFatalUnapply = Bind(bindSym,UnApply(ref(nonFatalUnapplySym), List(), List(ref(bindSym)), defn.NothingType))
        val errorCall = Apply(
          TypeApply(
            Select(trySupport(origin), "error".toTermName),
            List(TypeTree(exprUnwrappedType))
          ),
          List(ref(bindSym))
        )
        Try(expr, List(CaseDef(nonFatalUnapply, EmptyTree, errorCall)), EmptyTree)
  }


  private def wrapPureCpsTreeInTry(origin:Try, expr: CpsTree)(using Context, CpsTopLevelContext): Tree = {
    expr.asyncKind match
      case AsyncKind.Sync =>
        wrapPureExprTreeInTry(origin, expr.transformed, expr.originType.widen)
      case _ =>
        expr.transformed
  }

  private def trySupport(origin: Tree)(using CpsTopLevelContext): Tree = {
    val tctx = summon[CpsTopLevelContext]
    val trySupport = tctx.optTrySupport.getOrElse(
      throw CpsTransformException(s"try support is not enabled for ${tctx.monadType}", origin.srcPos)
    )
    trySupport
  }



}
