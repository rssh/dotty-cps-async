package cps

import scala.quoted._


trait CpsChunkBuilder[F[_],T]

  def create(): CpsChunk[F,T]

  def append[A:Type](chunk: CpsChunk[F,A]): CpsChunk[F,A]
  
  protected def fromFExpr(f: Expr[F[T]]): CpsChunk[F,T] =
          CpsChunk(Seq(),f)



object CpsChunkBuilder 

   def sync[F[_]:Type,T:Type](f:Expr[T], dm: Expr[AsyncMonad[F]])(given QuoteContext):CpsChunkBuilder[F,T] =
     new CpsChunkBuilder[F,T] {
        override def create() = fromFExpr('{ ${dm}.pure($f) })
        override def append[A:Type](e: CpsChunk[F,A]) = e.insertPrev(f)
     }
         

