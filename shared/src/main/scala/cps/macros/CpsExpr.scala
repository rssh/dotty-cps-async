// CPS Transform expression building block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021, 2022, 2023
package cps.macros

import scala.quoted._

import cps._
import cps.macros.common._
import cps.macros.misc._

import cps.macros.forest.*

trait ExprTreeGen:
  def extract(using Quotes): quotes.reflect.Statement
  def isChanged: Boolean

case class UnsealExprTreeGen[T](expr: Expr[T], changed: Boolean) extends ExprTreeGen:

  def extract(using Quotes): quotes.reflect.Statement =
    import quotes.reflect._
    expr.asTerm

  def isChanged: Boolean = changed

class StatementExprTreeGen(using Quotes)(stat: quotes.reflect.Statement, changed: Boolean) extends ExprTreeGen:
  def extract(using qctx:Quotes): quotes.reflect.Statement =
    stat.asInstanceOf[quotes.reflect.Statement]
  def isChanged: Boolean = changed


trait CpsExpr[F[_]:Type,T:Type](monad:Expr[CpsMonad[F]], prev: Seq[ExprTreeGen]):

  def isAsync: Boolean

  def isChanged: Boolean

  def fLast(using Quotes): Expr[F[T]]

  def transformed(using Quotes): Expr[F[T]] = {
     import quotes.reflect._
     val DEBUG = true
     if (prev.isEmpty)
       fLast
     else

       val lastTerm = fLast.asTerm 
       val lastOwners = TransformUtil.findOtherOwnersIn(lastTerm)

       val prevTerms = prev.map(_.extract).toList

       // black magic here, setting to Symbol.spliceOwner cause 
       //     ".... a reference to $sym was used outside the scope where it was defined" 
       // TODO: research
       val prevOwners = prevTerms.foldLeft(List.empty[Symbol]){ (s,e) =>
          e match
            case td: Definition =>
              if (td.symbol.maybeOwner != Symbol.noSymbol && td.symbol.maybeOwner != Symbol.spliceOwner ) {
                e.symbol.maybeOwner::s
              } else {
                s
              }
            case _ => s  
       }
       val newOwner = prevOwners.headOption.getOrElse(Symbol.spliceOwner)

       val changedPrevs = prevTerms.map{ e =>
         TransformUtil.reallyChangeOwner(e, newOwner).asInstanceOf[Statement]
       }

       val changedLast = lastTerm.changeOwner(newOwner)

       Block(changedPrevs, changedLast).asExprOf[F[T]]
  }


  def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T]

  def append[A:Type](chunk: CpsExpr[F,A])(using Quotes): CpsExpr[F,A]

  def syncOrigin(using Quotes): Option[Expr[T]]

  //"use monad instead")
  @Deprecated()
  def asyncMonad: Expr[CpsMonad[F]] = monad

  def tType: Type[T] = summon[Type[T]]

  def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[F,A] =
          MappedCpsExpr[F,T,A](monad,Seq(),this,f)

  def flatMap[A:Type](f: Expr[T => F[A]])(using Quotes): CpsExpr[F,A] =
          FlatMappedCpsExpr[F,T,A](monad,Seq(),this,f)

  def flatMapIgnore[A:Type](t: Expr[F[A]])(using Quotes): CpsExpr[F,A] =
         flatMap( '{ _ => $t } )

  def show(using Quotes): String =
    this.toString


abstract class SyncCpsExpr[F[_]:Type, T: Type](dm: Expr[CpsMonad[F]],
                                               prev: Seq[ExprTreeGen]) extends CpsExpr[F,T](dm, prev):

     override def isAsync = false

     def last(using Quotes): Expr[T]

     override def fLast(using Quotes): Expr[F[T]] =
          '{  ${dm}.pure(${last}:T) }

     override def syncOrigin(using Quotes): Option[Expr[T]] = 
       import quotes.reflect._
       Some(
         if prev.isEmpty then
            last
         else
            val lastTerm = last.asTerm
            val typedLast = if (lastTerm.tpe =:= TypeRepr.of[T]) {
                                lastTerm
                            } else {
                                Typed(lastTerm, TypeTree.of[T])
                            }
            Block(prev.toList.map(_.extract), typedLast).changeOwner(Symbol.spliceOwner).asExprOf[T]
       )



case class GenericSyncCpsExpr[F[_]:Type,T:Type](
                             dm: Expr[CpsMonad[F]],
                             prev: Seq[ExprTreeGen],
                             lastExpr: Expr[T],
                             changed: Boolean
                            ) extends SyncCpsExpr[F,T](dm, prev):

       override def isChanged = changed 

       def last(using Quotes): Expr[T] = lastExpr

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev,
                changed = changed || exprs.exists(_.isChanged))

       override def append[A:Type](e: CpsExpr[F,A])(using Quotes) =
           e.prependExprs(Seq(UnsealExprTreeGen(last, changed))).prependExprs(prev)

       override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[F,A] =
           copy(lastExpr = Expr.betaReduce('{$f($lastExpr)}), true )

       override def flatMap[A:Type](f: Expr[T => F[A]])(using Quotes): CpsExpr[F,A] =
            GenericAsyncCpsExpr[F,A](dm, prev, '{ $dm.flatMap($dm.pure($last))($f) } )


       override def flatMapIgnore[A:Type](t: Expr[F[A]])(using Quotes): CpsExpr[F,A] =
            GenericAsyncCpsExpr(dm, prev, '{ ${dm}.flatMap($dm.pure($last))(_ => $t) } )

       override def show(using Quotes) = {
         import quotes.reflect.*
         s"GenericSyncCpsExpr(dm=${dm.show},prev=$prev, lastExpr=${lastExpr.show}, changed=$changed)"
       }



abstract class AsyncCpsExpr[F[_]:Type,T:Type](
                              dm: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen]
                            ) extends CpsExpr[F,T](dm, prev):

       override def isAsync = true

       override def isChanged = true

       override def append[A:Type](e: CpsExpr[F,A])(using Quotes): CpsExpr[F,A] =
           flatMapIgnore(e.transformed)

       override def syncOrigin(using Quotes): Option[Expr[T]] = None



case class GenericAsyncCpsExpr[F[_]:Type,T:Type](
                            dm: Expr[CpsMonad[F]],
                            prev: Seq[ExprTreeGen],
                            fLastExpr: Expr[F[T]]
                            ) extends AsyncCpsExpr[F,T](dm, prev) {


    override def fLast(using Quotes): Expr[F[T]] = fLastExpr

    override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

    override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[F,A] =
           MappedCpsExpr(dm,Seq(),this,f)

    override def flatMap[A:Type](f: Expr[T => F[A]])(using Quotes): CpsExpr[F,A] =
           FlatMappedCpsExpr(dm,Seq(),this,f)

    override def flatMapIgnore[A:Type](t: Expr[F[A]])(using Quotes): CpsExpr[F,A] =
           FlatMappedCpsExpr(dm,Seq(),this, '{ (_:T) => $t })


    override def show(using Quotes) = {
      import quotes.reflect.*
      s"GenericAsyncCpsExpr(dm=${dm.show},prev=$prev, fLastExpr=${fLastExpr.show})"
    }

}



case class MappedCpsExpr[F[_]:Type, S:Type, T:Type](
                              monad: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen],
                              point: CpsExpr[F,S],
                              mapping: Expr[S=>T]
                              ) extends AsyncCpsExpr[F,T](monad, prev) {


  override def fLast(using Quotes): Expr[F[T]] =
                           '{ $monad.map(${point.transformed})($mapping) }

  override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
        if (exprs.isEmpty)
           this
        else
           copy(prev = exprs ++: prev)

  override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[F,A] =
      MappedCpsExpr[F,S,A](monad,prev,point, '{ x => $f($mapping(x)) })

  override def flatMap[A:Type](f: Expr[T => F[A]])(using Quotes): CpsExpr[F,A] =
      FlatMappedCpsExpr[F,S,A](monad, prev, point,
                                    '{ (x:S) => $f($mapping(x)) } )


}

case class FlatMappedCpsExpr[F[_]:Type, S:Type, T:Type](
                              monad: Expr[CpsMonad[F]],
                              prev: Seq[ExprTreeGen],
                              point: CpsExpr[F,S],
                              mapping: Expr[S => F[T]]
                             ) extends AsyncCpsExpr[F,T](monad, prev):

    override def fLast(using Quotes): Expr[F[T]] =
                             '{ ${monad}.flatMap(${point.transformed})($mapping) }

    override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)



case class UnitCpsExpr[F[_]:Type](monad: Expr[CpsMonad[F]],
                                  prev: Seq[ExprTreeGen],
                                  changed: Boolean)(using Quotes) extends SyncCpsExpr[F,Unit](monad,prev):

       override def isChanged = changed

       override def last(using Quotes): Expr[Unit] = '{()}

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,Unit] =
          if (exprs.isEmpty)
             this
          else
             copy(prev = exprs ++: prev,
                  changed = changed || exprs.exists(_.isChanged)
                 )

       override def append[A:Type](e: CpsExpr[F,A])(using Quotes) =
           if (prev.isEmpty)
             e
           else
             e.prependExprs(prev)


             
             

object CpsExpr:

   def sync[F[_]:Type,T:Type](dm: Expr[CpsMonad[F]], f: Expr[T], changed: Boolean): CpsExpr[F,T] =
     GenericSyncCpsExpr[F,T](dm, Seq(), f, changed)

   def async[F[_]:Type,T:Type](dm: Expr[CpsMonad[F]], f: Expr[F[T]]): CpsExpr[F,T] =
     GenericAsyncCpsExpr[F,T](dm, Seq(), f)

   def unit[F[_]:Type](dm: Expr[CpsMonad[F]])(using Quotes) =
     UnitCpsExpr[F](dm, Seq(), false)  //  GenericSyncCpsExpr[F,Unit](dm, Seq(), '{})

   def wrap[F[_]:Type, T:Type](internal:CpsExpr[F,T])(using Quotes): CpsExpr[F,T] =
      internal.syncOrigin match
         case Some(origin) => sync(internal.asyncMonad, origin, internal.isChanged)
         case None => async(internal.asyncMonad, internal.transformed)

