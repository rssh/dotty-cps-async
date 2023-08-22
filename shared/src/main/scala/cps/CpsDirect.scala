package cps

import scala.annotation.experimental
import scala.util.NotGiven
import cps.plugin.scaffolding.requiringCpsCompilerPlugin


/**
 *  Direct style context marker, used to mark function, whritten in direct style.
 *  When function have given parameter of type CpsDirect[F] or return context lambda with CpsDirect[F] parameter,
 *  then compiler plugin will transform function result into monadic form.
 *  For example:
 *  ```
 *   def fetch(url: String): CpsDirect[Future] ?=> String = ...
 *  ```
 **/
@experimental
class CpsDirect[F[_]](val context: CpsTryMonadContext[F]) extends  AnyVal {

  def monad: CpsMonad[F] = context.monad

  def throwMonad: CpsThrowMonad[F] = context.monad

  def tryMonad: CpsTryMonad[F] = context.monad

}


@experimental
object CpsDirect {

  inline given direct[F[_]](using context: CpsTryMonadContext[F]): CpsDirect[F] =
    new CpsDirect[F](context)
    //TODO: requiringCpsCompilerPlugin(new CpsDirect[F](context))


}

