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
     val cnBuild = new CpsChunkBuilder[F,T] {
           override def create() = 
              val fc = '{ ${dm}.pure(${f}) }
              CpsChunk(Seq(),fc)
           override def append[A:quoted.Type](e: CpsChunk[F,A]) = e.insertPrev(f)
     }
     CpsExprResult(f, cnBuild , tType, false)
  
  

