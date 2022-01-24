package cps.macros.forest

import scala.quoted._
import cps._
import cps.macros._


class ThisTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._

  def run(using Quotes)(thisTerm: quotes.reflect.This): CpsExpr[F,T] =
     CpsExpr.sync(monad, patternCode, false)

  
