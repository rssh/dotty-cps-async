package cps

import scala.quoted._


trait CpsChunkBuilder[F[_]:Type,T:Type]

  def create(): CpsChunk[F,T]

  def append[A:Type](chunk: CpsChunk[F,A]): CpsChunk[F,A]
  
  protected def fromFExpr(f: Expr[F[T]]): CpsChunk[F,T] =
          CpsChunk(Seq(),f)

  def pure[A:Type](monad: Expr[AsyncMonad[F]], t: Expr[A])(given QuoteContext): CpsChunk[F,A] = 
                           CpsChunk[F,A](Seq(),'{ ${monad}.pure(${t}) })

  def map[A:Type](monad: Expr[AsyncMonad[F]], t: Expr[T => A])(given QuoteContext): CpsChunk[F,A] =
                           CpsChunk[F,A](Seq(),'{ ${monad}.map(${create().toExpr})(${t}) })


  def flatMap[A:Type](monad: Expr[AsyncMonad[F]], t: Expr[T => F[A]])(given QuoteContext): CpsChunk[F,A] =
                 CpsChunk[F,A](Seq(), 
                      '{ ${monad}.flatMap(${create().toExpr})(${t}) }
                 )

  def flatMapIgnore[A:Type](monad: Expr[AsyncMonad[F]], t: Expr[F[A]])(given QuoteContext): CpsChunk[F,A] =
           CpsChunk[F,A](Seq(), 
                 '{ ${monad}.flatMap(${create().toExpr})(_ => ${t}) }
           )



object CpsChunkBuilder 

   def sync[F[_]:Type,T:Type](f:Expr[T], dm: Expr[AsyncMonad[F]])(given QuoteContext):CpsChunkBuilder[F,T] =
     new CpsChunkBuilder[F,T] {
        override def create() = fromFExpr('{ ${dm}.pure($f) })
        override def append[A:Type](e: CpsChunk[F,A]) = e.insertPrev(f)
     }
         

