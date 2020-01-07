package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class NewTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(tp: qctx.tasty.TypeTree): CpsExprResult[F,T] =
     val builder = CpsChunkBuilder.sync(patternCode,asyncMonad)
     CpsExprResult(patternCode, builder, patternType, false)

  


