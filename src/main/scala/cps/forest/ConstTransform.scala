package cps.forest

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps._


object ConstTransform 

  // we know, that f is match to Const
  //(see rootTransform)
  def apply[F[_]:Type,T:Type](transformationContext: TransformationContext[F,T])(
                                           given qctx: QuoteContext): CpsExprResult[F,T] =
     import transformationContext._
     val cnBuild = CpsChunkBuilder.sync(patternCode,asyncMonad) 
     CpsExprResult(patternCode, cnBuild, patternType, false)


