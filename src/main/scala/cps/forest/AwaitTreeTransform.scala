package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F, CT] =>
  
  import qctx.tasty.{_, given _}


  def runMyAwait(awaitTerm: Term, arg: Term): CpsTree =
      val cpsArg = runRoot(arg, TransformationContextMarker.Await)
      cpsArg.syncOrigin match
        case Some(sync) => AwaitCpsTree(sync, awaitTerm.tpe) 
        case None => cpsArg
      
  def runOtherAwait(awaitTerm: Term, arg: Term, targ: Type, otherCpsMonad: Term): CpsTree =
      val myCpsMonad = cpsCtx.monad.unseal
      val myCpsMonadTpe = myCpsMonad.tpe
      val myF = fType.unseal.tpe
      val otherF = targ
      val conversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
      val taConversion = AppliedType(conversion,List(otherF, myF))
      searchImplicit(taConversion) match
           case implSuccess: ImplicitSearchSuccess =>
             val convertedArg = Apply(
                    TypeApply(Select.unique(implSuccess.tree, "apply"), 
                              List(Inferred(arg.tpe.widen))),
                    List(otherCpsMonad, myCpsMonad, arg))
             runMyAwait(awaitTerm, convertedArg)
           case implFailure: ImplicitSearchFailure =>
             throw MacroError(s"Can't find MonadConversion: ${implFailure.explanation}",posExprs(awaitTerm))

  
