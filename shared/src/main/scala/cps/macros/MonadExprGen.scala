// CPS Transform expression building block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 20212022
package cps.macros

import scala.quoted.*
import scala.util.Try

import cps.*

trait MonadExprGen[F[_]:Type]:

  type Context

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]]

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]]
  
  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]]

  def supportsTryCatch(using Quotes): Boolean

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]]

  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]]

  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] 

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]]   

  def contextType: Type[Context]

  def applyGen[T](op: Expr[Context =>F[T]]): Expr[F[T]]

  def adoptAwait[A](c:Expr[Context],fa:Expr[F[A]]):Expr[F[A]]

  def show: String

end MonadExprGen

object MonadExprGen:

   type Aux[F[_],C] = MonadExprGen[F] { type Context = C }

end MonadExprGen


class CpsMonadExprGen[F[_]:Type,C:Type](dm: Expr[CpsMonad[F]]) extends MonadExprGen[F]:

  type Context = C 

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]] =
    '{  $dm.pure($t) }

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]] =
    '{ ${dm}.map($fa)($f) }

  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]] =
    '{ ${dm}.flatMap($fa)($f) }

  def supportsTryCatch(using Quotes): Boolean =
    import quotes.reflect.*
    dm.tpe <:< TypeRepr.of[CpsTryMonad[F]]

  def tryMonadExpr(using Quotes): Expr[CpsTryMonad[F]] =
    import quotes.reflect.*
    if (dm.tpe <:< TypeRepr.of{CpsTryMonad[F]}) {
       dm.asExprOf[CpsTryMonad[F]]
    } else {
       report.throwError("Monad is not supports try/catch")
    } 

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.mapTry($fa)($f) }

  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.flatMapTry($fa)($f) }
  
  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.restore($fa)($fx) }

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.withAction($fa)($action) }
  
  def applyGen[T](op: Expr[C =>F[T]]): Expr[F[T]] =
    '{ $dm.apply($op) }

  def adoptAwait[A](c:Expr[C],fa:Expr[F[A]]):Expr[F[A]] =
    '{ ${c}.adoptAwait(fa)  }

  def contextType: Type[Context] =
    summon[Type[C]]

end CpsMonadExprGen

