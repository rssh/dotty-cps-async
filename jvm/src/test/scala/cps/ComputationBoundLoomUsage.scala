package cps

import scala.quoted._

import cps.testconfig.given
import cps.runtime.Loom


object ComputationBoundLoomUsage {

   transparent inline def useLoom: Boolean = ${
      useLoomImpl
   }     

   def useLoomImpl(using Quotes): Expr[Boolean] = {
      val inLoom = Expr.summon[cps.macros.flags.UseLoomAwait.type]
      Expr(inLoom.isDefined)
   }



}