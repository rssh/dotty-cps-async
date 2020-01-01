package cps

import scala.quoted._

case class CpsChunk[F[_],T](prev: Seq[Expr[_]], last:Expr[F[T]]) 

    def toExpr(given QuoteContext): Expr[F[T]] =
      val nLast = last
      if (prev.isEmpty)
        nLast
      else
        Expr.block(prev.toList,nLast)

    def insertPrev[A](p: Expr[A]): CpsChunk[F,T] =
      CpsChunk(p +: prev, last) 
  


