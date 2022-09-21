// CPS Transform expression building block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021,2022
package cps.macros

import scala.quoted.*
import scala.util.Try

trait MonadExprGen[F[_]:Type]:

  type Context

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]]

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]]
  
  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]]

  def supportsTryCatch: Boolean

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]]

  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]]

  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] 

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]]   

  def applyGen[T](op: Expr[Context =>F[T]]): Expr[F[T]]

  def contextExprGen: MonadContextExprGen[F,Context]

  def show: String

end MonadExprGen

object MonadExprGen:

   type Aux[F[_],C] = MonadExprGen[F] { type Context = C }

end MonadExprGen


trait MonadContextExprGen[F[_]:Type, C:Type]:

  def adoptAwait[A](c:Expr[C],fa:Expr[F[A]]):F[A]

end MonadContextExprGen