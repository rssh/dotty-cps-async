package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._


class IdentTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):


  // case Ident(name) 
  def run(using Quotes)(name: String): CpsExpr[F,T] =
     import quotes.reflect._
     import cpsCtx._
     CpsExpr.sync(monad, patternCode, false) 
  
  

