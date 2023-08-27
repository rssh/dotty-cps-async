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
class CpsDirect[F[_]](val context: CpsTryMonadContext[F]) extends AnyVal {

  //def context: CpsTryMonadContext[F]

  def monad: CpsMonad[F] = context.monad

  def throwMonad: CpsThrowMonad[F] = context.monad

  def tryMonad: CpsTryMonad[F] = context.monad

}


//@experimental
//class CpsDirectInclusion[F[_],G[_]](val parentContext: CpsTryMonadContext[F], val inclusion: CpsTryMonadContextInclusion[F,G]) extends CpsDirect[G] {
//  def context: CpsTryMonadContext[G] = ???
//}

//@experimental
//class CpsDirectId[F[_]](override val context: CpsTryMonadContext[F]) extends AnyVal with CpsDirect[F]

//class CpsDirectConversion[F[_],G[_]](override val context: CpsTryMonadContext[F], val conversion: CpsTryMonadConversion[F,G]) extends CpsDirect[G]





@experimental
object CpsDirect  {

  @compileTimeOnly("CpsDirect consrtructioe should be removed by cps compiler plugin")
  given byInclusion[F[_], G[_]](using CpsTryMonadContext[F], CpsMonadContextInclusion[F,G]): CpsDirect[G] =
    ??? //CpsDirectId(await(summon[CpsMonad[G]].apply(ctx => ctx)))

  //direct[F[_]](using context: CpsTryMonadContext[F]): CpsDirect[F] =
  //  new CpsDirect[F](context)


}

trait CpsMonadContextInclusion[F[_],G[_]] {

  def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[G] => G[T]):F[T]

}

object CpsMonadContextInclusion {

  given byConversion[F[_],G[_]](using gfc: CpsMonadConversion[G,F], gm: CpsTryMonad[G]): CpsMonadContextInclusion[F,G] =
    new CpsMonadContextInclusion[F,G] {
      override def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[G] => G[T]):F[T] =
        gfc.apply(gm.apply(fun))
    }

  given byIdentity[F[_]]: CpsMonadContextInclusion[F,F] =
    new CpsMonadContextInclusion[F,F] {
      override def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[F] => F[T]): F[T] =
         fun(fctx)
    }

}