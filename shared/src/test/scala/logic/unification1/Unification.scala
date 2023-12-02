package logic.unification1

import cps._
import cps.stream._

import scala.util._


type UnificationEnvironment[R[+_]] = MultyUnificationEnvironment[R]

class LogicErrorException(msg: String, cause: Throwable=null) extends RuntimeException(msg, cause)

trait SingleUnificationEnvironment[R[+_]] extends CpsTryMonad[R] {

  def and[A,B](a:R[A],b:R[B]): R[(A,B)]

  def failure(reason: UnificationFailureReason = UnificationFailureReason.MISMATCH): R[Nothing]

  def success[T](value: T) : R[T]

  def logicError(msg: String): R[Nothing] = error(LogicErrorException(msg))

}




trait MultyUnificationEnvironment[R[+_]] extends SingleUnificationEnvironment[R] {

  def or[A](x:R[A]*): R[A]

}

trait AsyncUnificationEnvironment[R[+_]] extends MultyUnificationEnvironment[R] {

  type F[_]

  /**
   * Async input.  Allows to inco
   * @param value
   * @tparam A
   * @return
   */
  def adoptAsync[A](value: F[A]): R[A]

  def asyncOr[A](output: AsyncList[F,A]):R[A]

}

object AsyncUnificationEnvironment {

    type Aux[R[+_], FF[_]] = AsyncUnificationEnvironment[R] { type F[T]=FF[T] }

    type AuxF[FF[_]] = [R[+_]] =>> AsyncUnificationEnvironment[R] { type F[T]=FF[T] }

}



trait DelayUnificationEnvironment[R[+_]] extends UnificationEnvironment[R] {

  def waitResolved(v: LogicalTerm): R[LogicalTerm]

  def waitGround(v: LogicalTerm): R[LogicalTerm]

}


object UnificationSyntax {

  extension[R[+_]:SingleUnificationEnvironment, T:Unifiable] (self: R[T])
    def and[S](other: R[S]): R[(T,S)] = summon[SingleUnificationEnvironment[R]].and(self,other)

  def failure[R[+_]:SingleUnificationEnvironment](reason: UnificationFailureReason = UnificationFailureReason.MISMATCH): R[Nothing] = {
    summon[SingleUnificationEnvironment[R]].failure(reason)
  }

  inline def success[R[+_]:SingleUnificationEnvironment, T](value: T) : R[T] = {
    summon[SingleUnificationEnvironment[R]].success(value)
  }

  inline def logicError[R[+_]:SingleUnificationEnvironment](msg: String): R[Nothing] = {
    summon[SingleUnificationEnvironment[R]].logicError(msg)
  }

  extension[R[+_] : MultyUnificationEnvironment, T] (self: R[T])
    def or[S >: T](other: R[S]): R[S] = summon[MultyUnificationEnvironment[R]].or(self, other)

  def disjunction[R[+_] : MultyUnificationEnvironment, T](x: R[T]*): R[T] = {
    summon[MultyUnificationEnvironment[R]].or(x *)
  }

  def or[R[+_]:MultyUnificationEnvironment,T](x: Seq[R[T]]): R[T] = {
    summon[MultyUnificationEnvironment[R]].or(x *)
  }

  def waitResolved[R[+_]:DelayUnificationEnvironment](v: LogicalTerm): R[LogicalTerm] = {
    summon[DelayUnificationEnvironment[R]].waitResolved(v)
  }

  def asyncOr[R[+_]:AsyncUnificationEnvironment.AuxF[F],F[_]:CpsConcurrentMonad,T](x: AsyncList[F,T]): R[T] = {
    summon[AsyncUnificationEnvironment.Aux[R,F]].asyncOr(x)
  }

}
