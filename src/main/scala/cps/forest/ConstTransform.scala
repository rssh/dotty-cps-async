package cps.forest

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps._


object ConstTransform 

  // we know, that f is match to Const
  //(see rootTransform)
  def apply[F[_]:Type,T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]])(
                                           given qctx: QuoteContext): CpsExprResult[F,T] =
     val cnBuild = CpsChunkBuilder.sync(f,dm) 
     CpsExprResult(f, cnBuild , summon[Type[T]], false)


