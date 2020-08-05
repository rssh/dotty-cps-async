package cps

import scala.quoted._
import cps.misc._

trait ExprTreeGen:
  def extract(using qctx:QuoteContext): qctx.tasty.Statement

case class UnsealExprTreeGen[T](expr: Expr[T]) extends ExprTreeGen:
  def extract(using qctx:QuoteContext): qctx.tasty.Statement =
    expr.unseal

class StatementExprTreeGen(using qctx: QuoteContext)(stat: qctx.tasty.Statement) extends ExprTreeGen:
  def extract(using qctx:QuoteContext): qctx.tasty.Statement =
    stat.asInstanceOf[qctx.tasty.Statement]


trait CpsExpr[F[_]:Type,T:Type](monad:Expr[CpsMonad[F]], prev: Seq[ExprTreeGen]):

  def isAsync: Boolean

  def fLast(using QuoteContext): Expr[F[T]]

  def transformed(using qctx: QuoteContext): Expr[F[T]] =
     import qctx.tasty._
     if (prev.isEmpty)
       fLast
     else
       Block(prev.toList.map(_.extract), fLast.unseal).seal.cast[F[T]]

  def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T]

  def append[A:Type](chunk: CpsExpr[F,A])(using QuoteContext): CpsExpr[F,A]

  def syncOrigin(using QuoteContext): Option[Expr[T]]

  //"use monad instead")
  @Deprecated()
  def asyncMonad: Expr[CpsMonad[F]] = monad

  //def pure[A:Type](t: Expr[A])(using QuoteContext): CpsExpr[F,A] =
  //                     CpsExpr.sync(monad, t)

  def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
          MappedCpsExpr[F,T,A](monad,Seq(),this,f)

  def flatMap[A:Type](f: Expr[T => F[A]])(using QuoteContext): CpsExpr[F,A] =
          FlatMappedCpsExpr[F,T,A](monad,Seq(),this,f)

  def flatMapIgnore[A:Type](t: Expr[F[A]])(using QuoteContext): CpsExpr[F,A] =
         flatMap( '{ _ => $t } )




abstract class SyncCpsExpr[F[_]:Type, T: Type](dm: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen]) extends CpsExpr[F,T](dm, prev):

     override def isAsync = false

     def last(using QuoteContext): Expr[T]

     override def fLast(using QuoteContext): Expr[F[T]] =
          '{  ${dm}.pure(${last}) }

     override def syncOrigin(using qctx: QuoteContext): Option[Expr[T]] = Some(
         if prev.isEmpty then
            last
         else
            qctx.tasty.Block(prev.toList.map(_.extract), last.unseal).seal.asInstanceOf[Expr[T]]
     )



case class GenericSyncCpsExpr[F[_]:Type,T:Type](
                             dm: Expr[CpsMonad[F]],
                             prev: Seq[ExprTreeGen],
                             lastExpr: Expr[T]) extends SyncCpsExpr[F,T](dm, prev):


       def last(using QuoteContext): Expr[T] = lastExpr

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

       override def append[A:Type](e: CpsExpr[F,A])(using QuoteContext) =
           e.prependExprs(Seq(UnsealExprTreeGen(last))).prependExprs(prev)

       override def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
           copy(lastExpr = Expr.betaReduce('{$f($lastExpr)}) )

       override def flatMap[A:Type](f: Expr[T => F[A]])(using QuoteContext): CpsExpr[F,A] =
            GenericAsyncCpsExpr[F,A](dm, prev, '{ $dm.flatMap($dm.pure($last))($f) } )


       override def flatMapIgnore[A:Type](t: Expr[F[A]])(using QuoteContext): CpsExpr[F,A] =
            GenericAsyncCpsExpr(dm, prev, '{ ${dm}.flatMap($dm.pure($last))(_ => $t) } )




abstract class AsyncCpsExpr[F[_]:Type,T:Type](
                              dm: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen]
                            ) extends CpsExpr[F,T](dm, prev):

       override def isAsync = true

       override def append[A:Type](e: CpsExpr[F,A])(using QuoteContext): CpsExpr[F,A] =
           flatMapIgnore(e.transformed)

       override def syncOrigin(using QuoteContext): Option[Expr[T]] = None



case class GenericAsyncCpsExpr[F[_]:Type,T:Type](
                            dm: Expr[CpsMonad[F]],
                            prev: Seq[ExprTreeGen],
                            fLastExpr: Expr[F[T]]
                            ) extends AsyncCpsExpr[F,T](dm, prev) {


    override def fLast(using QuoteContext): Expr[F[T]] = fLastExpr

    override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

    override def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
           MappedCpsExpr(dm,Seq(),this,f)

    override def flatMap[A:Type](f: Expr[T => F[A]])(using QuoteContext): CpsExpr[F,A] =
           FlatMappedCpsExpr(dm,Seq(),this,f)

    override def flatMapIgnore[A:Type](t: Expr[F[A]])(using QuoteContext): CpsExpr[F,A] =
           FlatMappedCpsExpr(dm,Seq(),this, '{ (_:T) => $t })

}



case class MappedCpsExpr[F[_]:Type, S:Type, T:Type](
                              monad: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen],
                              point: CpsExpr[F,S],
                              mapping: Expr[S=>T]
                              ) extends AsyncCpsExpr[F,T](monad, prev) {


  override def fLast(using QuoteContext): Expr[F[T]] =
                             '{ $monad.map(${point.transformed})($mapping) }

  override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
        if (exprs.isEmpty)
           this
        else
           copy(prev = exprs ++: prev)

  override def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
      MappedCpsExpr[F,S,A](monad,prev,point, '{ x => $f($mapping(x)) })

  override def flatMap[A:Type](f: Expr[T => F[A]])(using QuoteContext): CpsExpr[F,A] =
      FlatMappedCpsExpr[F,S,A](monad, prev, point,
                                    '{ (x:S) => $f($mapping(x)) } )


}

case class FlatMappedCpsExpr[F[_]:Type, S:Type, T:Type](
                              monad: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen],
                              point: CpsExpr[F,S],
                              mapping: Expr[S => F[T]]
                             ) extends AsyncCpsExpr[F,T](monad, prev):

    override def fLast(using QuoteContext): Expr[F[T]] =
                             '{ ${monad}.flatMap(${point.transformed})($mapping) }

    override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

/*
case class AwaitAsyncCpsExpr[F[_]:Type,T:Type](
                              monad: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen],
                              nested: CpsExpr[F,F[T]]
                              ) extends AsyncCpsExpr[F,T](monad, prev):

    override def fLast(using QuoteContext) = ???

    override def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
           CpsExpr.impure()
*/
       


case class UnitCpsExpr[F[_]:Type](monad: Expr[CpsMonad[F]],
                                  prev: Seq[ExprTreeGen])(using QuoteContext) extends SyncCpsExpr[F,Unit](monad,prev):

       override def last(using QuoteContext): Expr[Unit] = '{()}

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,Unit] =
          if (exprs.isEmpty)
             this
          else
             copy(prev = exprs ++: prev)

       override def append[A:Type](e: CpsExpr[F,A])(using QuoteContext) =
           if (prev.isEmpty)
             e
           else
             e.prependExprs(prev)


object CpsExpr:

   def sync[F[_]:Type,T:Type](dm: Expr[CpsMonad[F]], f: Expr[T]): CpsExpr[F,T] =
     GenericSyncCpsExpr[F,T](dm, Seq(), f)

   def async[F[_]:Type,T:Type](dm: Expr[CpsMonad[F]], f: Expr[F[T]]): CpsExpr[F,T] =
     GenericAsyncCpsExpr[F,T](dm, Seq(), f)

   def unit[F[_]:Type](dm: Expr[CpsMonad[F]])(using QuoteContext) =
     UnitCpsExpr[F](dm, Seq())  //  GenericSyncCpsExpr[F,Unit](dm, Seq(), '{})

   def wrap[F[_]:Type, T:Type](internal:CpsExpr[F,T])(using QuoteContext): CpsExpr[F,T] =
      internal.syncOrigin match
         case Some(origin) => sync(internal.asyncMonad, origin)
         case None => async(internal.asyncMonad, internal.transformed)

