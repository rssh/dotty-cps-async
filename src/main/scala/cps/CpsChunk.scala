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
  


trait CpsChunkBuilder[F[_],T]

  def create(): CpsChunk[F,T]

  def append[A:Type](chunk: CpsChunk[F,A]): CpsChunk[F,A]
  
  protected def fromFExpr(f: Expr[F[T]]): CpsChunk[F,T] =
          CpsChunk(Seq(),f)

