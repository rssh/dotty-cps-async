package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait AwaitTreeTransform[F[_],CT, CC]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

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
      // TODO: skip call for implementation of CpsMonadInstanceContext  
      // 
      val adoptedArg = if (cpsCtx.monad.asTerm.tpe <:< TypeRepr.of[CpsMonadInstanceContext[?]]) {
                         arg
                       } else {  
                         adoptContextInMyAwait(awaitTerm, arg)
                       } 
      val cpsArg = runRoot(adoptedArg)
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

  def findContextObject(): Term =
    // TODO: cjeck at first that mcType is a context type [?]
    //  Check at first via implicit search, beacause we can have nested context functions inside async block,
    val monadContextClass = TypeIdent(Symbol.classSymbol("cps.CpsMonadContext")).tpe
    val fMonadContext = monadContextClass.appliedTo(List(TypeRepr.of[F]))
    Implicits.search(fMonadContext) match
      case implSuccess1: ImplicitSearchSuccess =>
        implSuccess1.tree
      case implFailure1: ImplicitSearchFailure =>
        val mcType = cpsCtx.monadContext.asTerm.tpe
        Implicits.search(mcType) match
          case implSuccess2: ImplicitSearchSuccess =>
             implSuccess2.tree
          case implFailure2: ImplicitSearchFailure =>
            // fallbact to monad context, but this
            if (cpsCtx.flags.debugLevel > 0) {
              cpsCtx.log(s"Can't find implicit ${fMonadContext.show} :${implFailure1.explanation}")
              cpsCtx.log(s"and ${mcType.show} :${implFailure2.explanation}")
            }
            // fallback, but if we here -- probably this is bug in dotty implicit resolution.
            // TODO: Implicits.search[CpsMonadContext[F]] not work, but we are situated inside 
            //       context function of ctx <: CpsMonadContext[F]. 
            //       fill bug report to dotty.
            // need research
            report.warning(s"Can't find none of ${mcType.show}, ${fMonadContext.show} via implicit search, fallback to async-wide context value")
            cpsCtx.monadContext.asTerm



  def adoptContextInMyAwait(awaitTerm: Term, arg: Term): Term =
    val monadContext = findContextObject()
    val adoptedArg = Apply(
      TypeApply(Select.unique(monadContext,"adoptAwait"), List(Inferred(awaitTerm.tpe.widen)) ),
      List(arg)
    )
    adoptedArg
