package cps.forest

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps._


object ConstTransform 

  // we know, that f is match to Const
  //(see rootTransform)
  def apply[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])(
                                           given qctx: QuoteContext): CpsChunkBuilder[F,T] =
     CpsChunkBuilder.sync(cpsCtx.asyncMonad, cpsCtx.patternCode) 


