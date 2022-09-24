package cps

/**
  * Minimal things that can be used for inferring monad context.
  * Used as common base for CpsMonad and InlineCpsMonad
  **/
trait CpsContextCarrier[F[_]]  {

  type Context <: CpsMonadContext[F]
 
  /**
    * run op in the context environment.
    **/
  def apply[T](op: Context => F[T]): F[T] 

  
}

object CpsContextCarrier {

  type Aux[F[_],C <: CpsMonadContext[F]] = CpsContextCarrier[F] { type Context = C } 

}