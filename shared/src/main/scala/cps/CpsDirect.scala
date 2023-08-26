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
trait CpsDirect[F[_]] {

  def context: CpsTryMonadContext[F]

  def monad: CpsMonad[F] = context.monad

  def throwMonad: CpsThrowMonad[F] = context.monad

  def tryMonad: CpsTryMonad[F] = context.monad

}

@experimental
class CpsDirectId[F[_]](override val context: CpsTryMonadContext[F]) extends CpsDirect[F]

@experimental
class CpsDirectConv[F[_],G[_]](val parentContext: CpsTryMonadContext[F], val conversion: CpsMonadConversion[F,G]) extends CpsDirect[G] {
  def context: CpsTryMonadContext[G] = ???
}

@experimental
trait CpsDirectLowLevel {

  given directWithConversion[F[_],G[_]](using parentContext: CpsTryMonadContext[F], conversion: CpsMonadConversion[F,G]): CpsDirect[G] =
    new CpsDirectConv[F,G](parentContext, conversion)
  
}


@experimental
object CpsDirect {

  // TODO: wrong position when inline
  given direct[F[_]](using context: CpsTryMonadContext[F]): CpsDirect[F] =
    new CpsDirectId[F](context)
    //TODO: requiringCpsCompilerPlugin(new CpsDirect[F](context))


}

