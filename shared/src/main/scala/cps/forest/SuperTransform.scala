package cps.forest

import scala.quoted._

import cps._


class SuperTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using Quotes)(superTerm: quotes.reflect.Super): CpsExpr[F,T] =
     CpsExpr.sync(monad, patternCode, false)

  
