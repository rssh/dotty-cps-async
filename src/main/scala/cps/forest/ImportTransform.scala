package cps.forest

import scala.quoted._

import cps._
import cps.misc._

//NOT USED
//TODO: remove

object ImportTransform:



  def fromBlock[F[_]:Type,T:Type](using qctx:QuoteContext)(cpsCtx: TransformationContext[F,T],
                           importTree: qctx.tasty.Import): CpsExpr[F,Unit] = {
     import qctx.tasty.{_, given _}
     import cpsCtx._
     if (cpsCtx.flags.debugLevel >= 10) {
       println(s"Import:fromBlock, importTree=$importTree")
     }
     val posExpr = Block(List(importTree),Literal(Constant(()))).seal
     // Import is not statement - so, it is impossible to create block with import in macros.
     //  from other side - all symbols on this stage are already resolved, so we can
     //  just erase import for our purpose.
     CpsExpr.unit(monad)
  }




              
