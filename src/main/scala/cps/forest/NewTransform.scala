package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class NewTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(tp: qctx.tasty.TypeTree): CpsChunkBuilder[F,T] =
     CpsChunkBuilder.sync(asyncMonad, patternCode)

  


