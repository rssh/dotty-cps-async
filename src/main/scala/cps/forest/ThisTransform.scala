package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class ThisTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(thisTerm: qctx.tasty.This): CpsExpr[F,T] =
     CpsExpr.sync(asyncMonad, patternCode)

  
