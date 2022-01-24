package cps.macros.forest

import scala.quoted._
import cps._
import cps.macros._
import cps.macros.misc._


class ReturnTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._

  def run(using Quotes)(returnTerm: quotes.reflect.Return, from: quotes.reflect.Symbol): CpsExpr[F,T] =
      throw MacroError("return inside asyn block is not supported",cpsCtx.patternCode)



  
