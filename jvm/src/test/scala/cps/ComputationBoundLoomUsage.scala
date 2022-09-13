package cps

import scala.quoted._

import cps.testconfig.given
import cps.runtime.Loom


object ComputationBoundLoomUsage {

   transparent inline def useLoomFast: Boolean = ${
      useLoomFastImpl
   }     

   def useLoomFastImpl(using Quotes): Expr[Boolean] = {
      val r = Expr.summon[cps.macros.flags.UseLoomAwait.type].isDefined && 
              Expr.summon[CpsFastRuntimeAwait[ComputationBound]].isDefined
      Expr(r)        
   }

   transparent inline def useLoomHybrid: Boolean = ${
      useLoomHybridImpl 
   }     

   def useLoomHybridImpl(using Quotes): Expr[Boolean] = {
      val r = Expr.summon[cps.macros.flags.UseLoomAwait.type].isDefined && 
               !Expr.summon[CpsFastRuntimeAwait[ComputationBound]].isDefined
      Expr(r)
   }

   transparent inline def useLoom: Boolean = ${
      useLoomImpl 
   }     


   def useLoomImpl(using Quotes): Expr[Boolean] = {
      val inLoom = Expr.summon[cps.macros.flags.UseLoomAwait.type]
      Expr(inLoom.isDefined)
   }



}