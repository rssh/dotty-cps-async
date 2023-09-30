package cps


/**
 * When this typeclass is implemented for a monad F, 
 * dotty-cps-async can use runtime await invocations
 * for handling of high-order functions when shifted variants are not available. 
 **/
trait CpsRuntimeAwait[F[_]] {

    def await[A](fa: F[A])(ctx: CpsTryMonadContext[F]): A

}

/**
 * Indirect constructor for CpsRuntimeAwait[F] instance, which can be used in situation where
 * runtime await instance can be build only inside of monad.  (Example - cats.effect.IO)
 *
 * When this typeclass is implemented for a monad F, we also can process arguments of high-order functions
 * wihout requiring of shifted variants.
 */
trait CpsRuntimeAwaitProvider[F[_]] {

  def withRuntimeAwait[A](lambda: CpsRuntimeAwait[F] => F[A])(using ctx:CpsTryMonadContext[F]): F[A]

}


/**
 * Marker class which mean that CpsRuntimeAwait implemented in such way,
 * that performance penalty in comparison with cps run is relative low
 * and we can not to use cps transformation in async block for such monad.
 **/
trait CpsFastRuntimeAwait[F[_]] extends CpsRuntimeAwait[F]


