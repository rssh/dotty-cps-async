package cps.macros.forest

import scala.quoted._
import cps._
import cps.macros._
import cps.macros.misc._

//NOT USED
//TODO: remove

object ImportTransform:



  def fromBlock[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](using Quotes)(cpsCtx: TransformationContext[F,T,C],
                           importTree: quotes.reflect.Import): CpsExpr[F,Unit] = {
     import quotes.reflect._
     import cpsCtx._
     if (cpsCtx.flags.debugLevel >= 10) {
       println(s"Import:fromBlock, importTree=$importTree")
     }
     val posExpr = Block(List(importTree),Literal(UnitConstant())).asExpr
     // Import is not statement - so, it is impossible to create block with import in macros.
     //  from other side - all symbols on this stage are already resolved, so we can
     //  just erase import for our purpose.
     CpsExpr.unit(monad)
  }





