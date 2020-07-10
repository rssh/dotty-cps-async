package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F, CT] =>
  
  import qctx.tasty.{_, given _}


  def runMyAwait(awaitTerm: Term, arg: Term): CpsTree =
      AwaitCpsTree(arg, awaitTerm.tpe) 
      
  def runOtherAwait(awaitTerm: Term, arg: Term, targs: Type, otherCpsMonad: Term): CpsTree =
      val conversion = TypeIdent(Symbol.classSymbol("scala.Conversion")).tpe
      val inOtherMonad = arg.tpe.widen
      val inMyMonad = AppliedType(monadTypeTree.tpe,List(awaitTerm.tpe.widen))
      val taConversion = AppliedType(conversion,List(inOtherMonad, inMyMonad))
         //val taConversion = '[scala.Conversion[$inOtherMonad,$inMyMonad]].unseal.tpe
      searchImplicit(taConversion) match
           case implSuccess: ImplicitSearchSuccess =>
             val convertedAwait = Apply(implSuccess.tree, List(arg))
             AwaitCpsTree(convertedAwait, awaitTerm.tpe )
           case implFailure: ImplicitSearchFailure =>
             println("!!!after searchImplicit [not found]")
             throw MacroError(s"Can't find Conversion from ${inOtherMonad.show} to ${inMyMonad.show} (${implFailure.explanation}) ",posExprs(awaitTerm))



