package cps

import cps.*
import cps.monads.{*, given}

trait CpsSyncLogicMonad[M[_]] extends CpsLogicMonad[M] {

  override type Observer[T] = T

  override def observerCpsMonad: CpsTryMonad[Observer] = CpsIdentityMonad
  
  def toLazyList[T](m:M[T]): LazyList[T]
  
}


