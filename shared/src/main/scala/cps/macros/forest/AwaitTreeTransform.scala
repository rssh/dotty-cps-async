package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


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
      val cpsArg = runRoot(arg)
      cpsArg.applyAwait(awaitTerm.tpe)

  def runOtherAwait(awaitTerm: Term, arg: Term, targ: TypeRepr, otherCpsMonad: Term): CpsTree =
      val myCpsMonad = cpsCtx.monad.asTerm
      val myCpsMonadTpe = myCpsMonad.tpe
      val myF = TypeRepr.of[F]
      val otherF = targ
      val tTpe = awaitTerm.tpe.widen
      //val conversion = TypeIdent(Symbol.classSymbol("scala.Conversion")).tpe
      //val taConversion = conversion.appliedTo(List(otherF.appliedTo(tTpe), myF.appliedTo(tTpe)))
      val monadConversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
      val taConversion = monadConversion.appliedTo(List(otherF, myF))
      Implicits.search(taConversion) match
           case implSuccess: ImplicitSearchSuccess =>
             //val convertedArg = Apply(Select.unique(implSuccess.tree, "apply"),List(arg))
             val convertedArg = Apply(TypeApply(Select.unique(implSuccess.tree, "apply"),List(Inferred(tTpe))),List(arg))
             runMyAwait(awaitTerm, convertedArg)
           case implFailure: ImplicitSearchFailure =>
             throw MacroError(s"Can't find ${taConversion.show}: ${implFailure.explanation}",posExprs(awaitTerm))

