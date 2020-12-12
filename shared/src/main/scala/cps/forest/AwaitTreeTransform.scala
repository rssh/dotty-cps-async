package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  def runAwait(term: Term, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term): CpsTree =
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait, arg=${arg.show}")
      val r = if awaitCpsMonadType =:= monadTypeTree.tpe then
        runMyAwait(term, arg)
      else
        runOtherAwait(term, arg, awaitCpsMonadType, awaitCpsMonad)
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait result=${r}")
      r


  def runMyAwait(awaitTerm: Term, arg: Term): CpsTree =
      val cpsArg = runRoot(arg, TransformationContextMarker.Await)
      cpsArg.applyAwait(awaitTerm.tpe)
      /*
      cpsArg.syncOrigin match
        case Some(sync) => AwaitSyncCpsTree(sync, awaitTerm.tpe)
        case None =>
             //AwaitAsyncCpsTree(cpsArg, awaitTerm.tpe)
             cpsArg.applyAwait()
      */

  def runOtherAwait(awaitTerm: Term, arg: Term, targ: TypeRepr, otherCpsMonad: Term): CpsTree =
      val myCpsMonad = cpsCtx.monad.asTerm
      val myCpsMonadTpe = myCpsMonad.tpe
      val myF = TypeRepr.of[F]
      val otherF = targ
      val conversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
      val taConversion = conversion.appliedTo(List(otherF, myF))
      Implicits.search(taConversion) match
           case implSuccess: ImplicitSearchSuccess =>
             val convertedArg = Apply(
                    TypeApply(Select.unique(implSuccess.tree, "apply"),
                              List(Inferred(awaitTerm.tpe))),
                    List(otherCpsMonad, myCpsMonad, arg))
             runMyAwait(awaitTerm, convertedArg)
           case implFailure: ImplicitSearchFailure =>
             throw MacroError(s"Can't find MonadConversion: ${implFailure.explanation}",posExprs(awaitTerm))
