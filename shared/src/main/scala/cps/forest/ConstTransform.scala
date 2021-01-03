package cps.forest

import scala.quoted._
import scala.compiletime._

import cps._


object ConstTransform:

  // we know, that f is match to Const
  //(see rootTransform)
  def run[F[_]:Type,T:Type](using Quotes)(cpsCtx: TransformationContext[F,T],
                                          constTerm: quotes.reflect.Literal):CpsExpr[F,T] =
     CpsExpr.sync(cpsCtx.monad, cpsCtx.patternCode) 


