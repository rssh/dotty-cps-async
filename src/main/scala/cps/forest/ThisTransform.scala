package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class ThisTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(thisTerm: qctx.tasty.This): CpsExpr[F,T] =
     CpsExpr.sync(monad, patternCode)

  
