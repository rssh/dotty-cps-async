package cps.monads

import scala.util._
import scala.util.control._
import scala.util.control.TailCalls._
import scala.concurrent._

import cps._


given CpsThrowMonad[TailRec] with CpsThrowMonadInstanceContext[TailRec] with {

  def pure[A](a:A): TailRec[A] =
    done(a)

  def map[A,B](fa: TailRec[A])(f: A=>B): TailRec[B] =
    fa.map(f)

  def flatMap[A,B](fa: TailRec[A])(f: A=>TailRec[B]): TailRec[B] =
    fa.flatMap(f)

  def error[A](e: Throwable): TailRec[A] =
    done(throw e)


}
