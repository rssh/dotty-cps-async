package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class IdentTransform[F[_]:Type, T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]])


  // case Ident(name) 
  def run(given qctx: QuoteContext)(name: String): CpsExprResult[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     println(s"!!! ident detected : ${f}")
     val cnBuild = CpsChunkBuilder.sync(f,dm) 
     CpsExprResult(f, cnBuild , tType, false)
  
  

