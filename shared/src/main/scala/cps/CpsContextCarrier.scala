package cps

/**
 * Minimal things that can 
 **/
trait CpsContextCarrier[F[_]] extends CpsAwaitable[F] {

  type Context <: CpsMonadContext[F]
  

}