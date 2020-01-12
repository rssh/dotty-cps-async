package cps

import scala.quoted._


trait CpsChunkBuilder[F[_]:Type,T:Type](monad:Expr[AsyncMonad[F]])

  def create(): CpsChunk[F,T]

  def append[A:Type](chunk: CpsChunk[F,A]): CpsChunk[F,A]
  
  protected def fromFExpr(f: Expr[F[T]]): CpsChunk[F,T] =
          CpsChunk(Seq(),f)

  def pure[A:Type](t: Expr[A])(given QuoteContext): CpsChunk[F,A] = 
                           CpsChunk[F,A](Seq(),'{ ${monad}.pure(${t}) })

  def map[A:Type](t: Expr[T => A])(given QuoteContext): CpsChunk[F,A] =
                           CpsChunk[F,A](Seq(),'{ ${monad}.map(${create().toExpr})(${t}) })


  def flatMap[A:Type](t: Expr[T => F[A]])(given QuoteContext): CpsChunk[F,A] =
                 CpsChunk[F,A](Seq(), 
                      '{ ${monad}.flatMap(${create().toExpr})(${t}) }
                 )

  def flatMapIgnore[A:Type](t: Expr[F[A]])(given QuoteContext): CpsChunk[F,A] =
           CpsChunk[F,A](Seq(), 
                 '{ ${monad}.flatMap(${create().toExpr})(_ => ${t}) }
           )



object CpsChunkBuilder 

   def sync[F[_]:Type,T:Type](f:Expr[T], dm: Expr[AsyncMonad[F]])(given QuoteContext):CpsChunkBuilder[F,T] =
     new CpsChunkBuilder[F,T](dm) {
        override def create() = fromFExpr('{ ${dm}.pure($f) })
        override def append[A:Type](e: CpsChunk[F,A]) = e.insertPrev(f)
     }
         
   def async[F[_]:Type,T:Type](dm: Expr[AsyncMonad[F]], f: Expr[F[T]])(given QuoteContext):
                                                                               CpsChunkBuilder[F,T] = 
     new CpsChunkBuilder[F,T](dm) {
        override def create() = fromFExpr(f)
        override def append[A:Type](e: CpsChunk[F,A]) = 
                          flatMapIgnore(e.toExpr)
     }



