package cps.monads.logic

import cps.*
import cps.monads.{*, given}

trait CpsSyncLogicMonad[M[_]] extends CpsLogicMonad[M] {

  override type Observer[T] = T

  override val observerCpsMonad: CpsTryMonad[Observer] = CpsIdentityMonad

  def toLazyList[T](m:M[T]): LazyList[T]

}


extension [M[_]:CpsSyncLogicMonad,A](ma:M[A]) 

  def toLazyList: LazyList[A] =
    summon[CpsSyncLogicMonad[M]].toLazyList(ma)