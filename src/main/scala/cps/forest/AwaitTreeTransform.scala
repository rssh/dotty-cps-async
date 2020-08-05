package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_, given _}


  def runMyAwait(awaitTerm: Term, arg: Term): CpsTree =
      AwaitCpsTree(arg, awaitTerm.tpe)

  def runOtherAwait(awaitTerm: Term, arg: Term, targ: Type, otherCpsMonad: Term): CpsTree =
      val myCpsMonad = cpsCtx.monad.asTerm
      val myCpsMonadTpe = myCpsMonad.tpe
      val myF = fType.asTypeTree.tpe
      val otherF = targ
      val conversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
      val taConversion = AppliedType(conversion,List(otherF, myF))
      searchImplicit(taConversion) match
           case implSuccess: ImplicitSearchSuccess =>
             val convertedAwait = Apply(
                    TypeApply(Select.unique(implSuccess.tree, "apply"),
                              List(Inferred(arg.tpe.widen))),
                    List(otherCpsMonad, myCpsMonad, arg))
             AwaitCpsTree(convertedAwait, awaitTerm.tpe )
           case implFailure: ImplicitSearchFailure =>
             println("!!!after searchImplicit [not found]")
             throw MacroError(s"Can't find MonadConversion: ${implFailure.explanation}",posExprs(awaitTerm))




