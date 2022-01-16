package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._


class IdentTransform[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):


  // case Ident(name) 
  def run(using Quotes)(name: String): CpsExpr[F,T] =
     import quotes.reflect._
     import cpsCtx._
     CpsExpr.sync(monad, patternCode, false) 
  
  

