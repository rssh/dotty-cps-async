package cps.macros.forest

import scala.quoted._
import scala.util.control.NonFatal

import cps._
import cps.macros._
import cps.macros.misc._



trait AwaitTreeTransform[F[_],CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  def runAwait(term: Term, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonadConversion: Term, awaitCpsMonadContext: Term)(owner:Symbol): CpsTree =
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait, arg=${arg.show}, awaitCpsMonadConversion = ${awaitCpsMonadConversion.show} awaitCpsMonadContext=${awaitCpsMonadContext.show}")
          cpsCtx.log(s"runAwait, argTree=${arg.show}")

      val r = if awaitCpsMonadType =:= monadTypeTree.tpe then
        runMyAwait(term, arg, awaitCpsMonadContext)(owner)
      else
        runOtherAwait(term, arg, awaitCpsMonadType, awaitCpsMonadConversion, awaitCpsMonadContext)(owner)
      if cpsCtx.flags.debugLevel >= 10 then
          cpsCtx.log(s"runAwait result=${r}")
      r


  def runMyAwait(awaitTerm: Term, arg: Term, context: Term)(owner:Symbol): CpsTree =
      //val adoptedArg = if (context.tpe <:< TypeRepr.of[CpsMonadNoAdoptContext[?]]) {
      //                   arg
      //                 } else {
      //                   adoptContextInMyAwait(awaitTerm, arg, context)
      //                 }
      //val cpsArg = runRoot(adoptedArg)(owner)
      //  now we have no adoptContext, instead assume that monad is proxied via context.
      val cpsArg = runRoot(arg)(owner)
      cpsArg.applyAwait(awaitTerm.tpe)

  def runOtherAwait(awaitTerm: Term, arg: Term, targ: TypeRepr, awaitCpsMonadConversion: Term, myMonadContext: Term)(owner: Symbol): CpsTree =

      //val myCpsMonad = cpsCtx.monad.asTerm
      //val myCpsMonadTpe = myCpsMonad.tpe
      val myF = TypeRepr.of[F]
      val otherF = targ
      val tTpe = awaitTerm.tpe.widen
      //val conversion = TypeIdent(Symbol.classSymbol("scala.Conversion")).tpe
      //val taConversion = conversion.appliedTo(List(otherF.appliedTo(tTpe), myF.appliedTo(tTpe)))
      val monadConversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
      // TODO: check for identity conversion and optimize one out
      val convertedArg = Apply(TypeApply(Select.unique(awaitCpsMonadConversion, "apply"), List(Inferred(tTpe))), List(arg))
      runMyAwait(awaitTerm, convertedArg, myMonadContext)(owner)



  def adoptContextInMyAwait(awaitTerm: Term, arg: Term, monadContext: Term): Term =
    //val monadContext = findContextObject()
    try {
      val adoptedArg = Apply(
        TypeApply(Select.unique(monadContext, "adoptAwait"), List(Inferred(awaitTerm.tpe.widen))),
        List(arg)
      )
      adoptedArg
    } catch {
      case ex: Throwable =>
        println(s"exception in await, monadContext=${monadContext.show}, arg=${arg.show}, pos=${awaitTerm.pos}")
        report.error(s"exception in await, monadContext=${monadContext.show}, arg=${arg.show}",awaitTerm.pos)
        throw ex
    }

