package cps.forest

import scala.quoted._

import cps._


class SuperTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(superTerm: qctx.reflect.Super): CpsExpr[F,T] =
     CpsExpr.sync(monad, patternCode)

  
