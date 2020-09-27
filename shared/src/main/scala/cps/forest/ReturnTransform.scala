package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class ReturnTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(returnTerm: qctx.tasty.Return): CpsExpr[F,T] =
      throw MacroError("return inside asyn block is not supported",cpsCtx.patternCode)



  
