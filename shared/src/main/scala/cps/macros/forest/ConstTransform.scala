package cps.macros.forest

import scala.quoted._
import scala.compiletime._

import cps._
import cps.macros._


object ConstTransform:

  // we know, that f is match to Const
  //(see rootTransform)
  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](using Quotes)(cpsCtx: TransformationContext[F,T,C],
                                          constTerm: quotes.reflect.Literal):CpsExpr[F,T] =
     import quotes.reflect._
     if (cpsCtx.flags.debugLevel >= 10) then
        cpsCtx.log(s"const: T=${Type.show[T]}, code=${cpsCtx.patternCode.show}")
     //CpsExpr.sync(cpsCtx.monad, Typed(constTerm, TypeTree.of[T] ).asExprOf[T], true) 
     // policy: where to insert typed[?]
     CpsExpr.sync(cpsCtx.monad, constTerm.asExprOf[T], false) 


