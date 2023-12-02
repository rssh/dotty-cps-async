package logic.slm

import cps.*
import cps.stream.AsyncList

import scala.util.*
import scala.util.control.NonFatal

class ContradictionException extends RuntimeException

trait Deductive[F[_]:CpsConcurrentMonad,X] {

    def result: AsyncList[F,Try[X]]

    def insert(x:X): Deductive[F,X] = {
      val oldResult = result
      new Deductive[F,X] {
        def result = AsyncList.Cons(Success(x), () => oldResult)
      }
    }

    def or[Y >: X](other: => Deductive[F,Y]): Deductive[F,Y] = {
      val xResult = result
      new Deductive[F,Y] {
        def result = xResult.merge[Try[Y]](other.result)
      }
    }

    def map[Y](f:X=>Y): Deductive[F,Y] =
      Deductive.Map(this,f)

    def flatMap[Y](f:X=>Deductive[F,Y]): Deductive[F,Y] =
      Deductive.FlatMap(this,f)

    def flatMapTry[Y](f:Try[X]=>Deductive[F,Y]): Deductive[F,Y] =
      Deductive.FlatMapTry(this,f)

}

object Deductive {

  case class Pure[F[_]:CpsConcurrentMonad,X](x:X) extends Deductive[F,X] {
    def result = AsyncList.Cons(Success(x), () => AsyncList.empty[F])
  }

  case class Empty[F[_]:CpsConcurrentMonad,X]() extends Deductive[F,X] {
    def result = AsyncList.empty[F]
  }

  case class Map[F[_]:CpsConcurrentMonad,X,Y](source:Deductive[F,X], f:X=>Y) extends Deductive[F,Y] {
    def result = source.result.map(_.map(f))
  }

  case class FlatMap[F[_]:CpsConcurrentMonad,X,Y](source:Deductive[F,X], f:X=>Deductive[F,Y]) extends Deductive[F,Y] {
    def result = source.result.flatMap { x =>
      x match
        case Success(x) =>
          tryWithError(f(x).result)
        case Failure(ex) =>
          errorResult(ex)
    }
  }

  case class Error[F[_]:CpsConcurrentMonad,X](ex:Throwable) extends Deductive[F,X] {
    def result = AsyncList.Cons(Failure(ex), () => AsyncList.empty[F])
  }

  case class FlatMapTry[F[_]:CpsConcurrentMonad,X,Y](source:Deductive[F,X], f:Try[X]=>Deductive[F,Y]) extends Deductive[F,Y] {
    def result = source.result.flatMap { x =>
      tryWithError(f(x).result)
    }
  }

  def tryWithError[F[_],T](op: => AsyncList[F,Try[T]])(using CpsConcurrentMonad[F]): AsyncList[F,Try[T]] =
    try
       op
    catch
      case ex:Throwable =>
        errorResult(ex)

  def errorResult[F[_],T](ex:Throwable)(using CpsConcurrentMonad[F]): AsyncList[F,Try[T]] = {
    if (ex.isInstanceOf[ContradictionException]) {
      AsyncList.empty[F]
    } else {
      AsyncList.Cons(Failure(ex), () => AsyncList.empty[F])
    }
  }

  def pure[F[_]:CpsConcurrentMonad,X](x:X): Deductive[F,X] =
    Pure(x)

  def error[F[_]:CpsConcurrentMonad,X](ex:Throwable): Deductive[F,X] =
    Error(ex)

  class DeductiveContext[F[_]](m:DeductiveCpsMonad[F]) extends CpsTryMonadContext[[X]=>>Deductive[F,X]] {

    override def monad: CpsTryMonad[[X]=>>Deductive[F,X]] = m


  }

  class DeductiveCpsMonad[F[_]:CpsConcurrentMonad] extends CpsTryContextMonad[[X]=>>Deductive[F,X],DeductiveContext[F]] {

    def pure[T](t:T): Deductive[F,T] =
      Pure(t)

    def error[T](ex:Throwable): Deductive[F,T] =
      Error(ex)

    def flatMap[A,B](fa:Deductive[F,A])(f:A=>Deductive[F,B]): Deductive[F,B] =
      fa.flatMap(f)

    def map[A,B](fa:Deductive[F,A])(f:A=>B): Deductive[F,B] =
      fa.map(f)

    def flatMapTry[A,B](fa:Deductive[F,A])(f:Try[A]=>Deductive[F,B]): Deductive[F,B] =
      fa.flatMapTry(f)

    override def applyContext[T](op: DeductiveContext[F] => Deductive[F, T]): Deductive[F, T] = {
      val ctx = new DeductiveContext[F](this)
      op(ctx)
    }

  }

  given deductiveCpsMonad[F[_]:CpsConcurrentMonad]: DeductiveCpsMonad[F] = DeductiveCpsMonad[F]()

}


