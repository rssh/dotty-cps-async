package cps


//
//  map: A=>B
//  map_async: A=>F[B] ???
//  f': A=>F[B],    f'':  A=>runtime.await(F[B]) 

/**
 * When this typeclass is implemented for a monad F, 
 * dotty-cps-async can use runtime await invocations
 * for handling of high-order functions when shifted variants are not available. 
 **/
trait CpsRuntimeAwait[F[_]] {

    def async[A,C <: CpsTryMonadContext[F]](f: C=>A)(m: CpsAsyncEffectMonad[F], ctx:C):F[A] = {
      m.flatDelay(runAsync(f)(m,ctx))
    }
    
    def runAsync[A,C <: CpsTryMonadContext[F]](f: C=>A)(m: CpsAsyncEffectMonad[F], ctx:C):F[A]

    def await[A](fa: F[A])(ctx: CpsTryMonadContext[F]): A

}

/**
 * Marker class which mean that CpsRuntimeAwait implemented in such way,
 * that performance penalty in comparison with cps run is relative low
 * and we can not to use cps transformation in async block for such monad.
 **/
trait CpsFastRuntimeAwait[F[_]] extends CpsRuntimeAwait[F]


