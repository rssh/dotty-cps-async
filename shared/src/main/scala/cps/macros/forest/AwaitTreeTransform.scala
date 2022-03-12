package cps.macros.forest

import scala.quoted._
import scala.util.control.NonFatal

import cps._
import cps.macros._
import cps.macros.misc._



trait AwaitTreeTransform[F[_],CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  def runAwait(term: Term, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, awaitCpsMonadContext: Term): CpsTree =
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait, arg=${arg.show}")
      val r = if awaitCpsMonadType =:= monadTypeTree.tpe then
        runMyAwait(term, arg, awaitCpsMonadContext)
      else
        runOtherAwait(term, arg, awaitCpsMonadType, awaitCpsMonad, awaitCpsMonadContext)
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait result=${r}")
      r


  def runMyAwait(awaitTerm: Term, arg: Term, context: Term): CpsTree =
      val adoptedArg = if (context.tpe <:< TypeRepr.of[CpsMonadNoAdoptContext[?]]) {
                         arg
                       } else {  
                         adoptContextInMyAwait(awaitTerm, arg, context)
                       } 
      val cpsArg = runRoot(adoptedArg)
      cpsArg.applyAwait(awaitTerm.tpe)

  def runOtherAwait(awaitTerm: Term, arg: Term, targ: TypeRepr, otherCpsMonad: Term, myMonadContext: Term): CpsTree =
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
             runMyAwait(awaitTerm, convertedArg, myMonadContext)
           case implFailure: ImplicitSearchFailure =>
             val taConversionPrinted = try {
               taConversion.show
             } catch {
               case NonFatal(ex) =>
                taConversion.toString
             }
             throw MacroError(s"Can't find ${taConversionPrinted}: ${implFailure.explanation}",posExprs(awaitTerm))


  def adoptContextInMyAwait(awaitTerm: Term, arg: Term, monadContext: Term): Term =
    //val monadContext = findContextObject()
    val adoptedArg = Apply(
      TypeApply(Select.unique(monadContext,"adoptAwait"), List(Inferred(awaitTerm.tpe.widen)) ),
      List(arg)
    )
    adoptedArg
