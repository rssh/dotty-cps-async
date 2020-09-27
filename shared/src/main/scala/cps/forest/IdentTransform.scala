package cps.forest

import scala.quoted._

import cps._


class IdentTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):


  // case Ident(name) 
  def run(using qctx: QuoteContext)(name: String): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     import cpsCtx._
     CpsExpr.sync(monad, patternCode) 
  
  

