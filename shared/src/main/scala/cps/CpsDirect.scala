package cps

import scala.annotation.{compileTimeOnly, experimental}
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
type CpsDirect[F[_]] = CpsDirect.Direct[F]


@experimental
object CpsDirect  {

  opaque type Direct[F[_]] = CpsTryMonadContext[F]
  
  private[cps] def apply[F[_]](ctx: CpsTryMonadContext[F]): CpsDirect[F] = ctx

  extension [F[_]](x: CpsDirect[F])
    def context: CpsTryMonadContext[F] = x
    def monad: CpsTryMonad[F] = x.context.monad
    def throwMonad: CpsTryMonad[F] = x.context.monad
    def tryMonad: CpsTryMonad[F] = x.context.monad

  @compileTimeOnly("CpsDirect consrtuction should be removed by cps compiler plugin, make sure that cos plugin is enabled")
  given byInclusion[F[_], G[_]](using CpsTryMonadContext[F], CpsMonadContextInclusion[F,G]): CpsDirect[G] =
    ???

}

