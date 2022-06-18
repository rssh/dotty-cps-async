package cps

import scala.compiletime.*
import scala.util.NotGiven



/**
 * Base for context operations inside monad
 **/
trait CpsMonadContext[F[_]] {

  type Monad[X] = F[X]

  /**
   * adopt external monadic value to the current context.
   **/
  def adoptAwait[A](fa:F[A]):F[A]
 

}

/**
 * marker trait for context with NOOP intercaprAwait operation 
 **/
trait CpsMonadNoAdoptContext[F[_]] extends CpsMonadContext[F] {

  /**
   * If is it statically known, that monad is evaluated in this context, then
   * this call is completely eliminated by dotty-cps-async macro
   *@return fa
   **/
   def adoptAwait[A](fa:F[A]):F[A] = fa

} 


class CpsMonadInstanceContextBody[F[_]](m: CpsMonadInstanceContext[F]) extends CpsMonadNoAdoptContext[F] {


   def monad: CpsMonad[F] = m

}


/**
 * Trait for minimal monad context, which provides an instance of CpsMonad.
 * Mixin this trait into your monad in cases, when you monad have no internal API.
 **/
trait CpsMonadInstanceContext[F[_]] extends CpsMonad[F] {


  type Context = CpsMonadInstanceContextBody[F]

  /**
  * run with this instance
  **/
  def apply[T](op: Context => F[T]): F[T] =
    op(CpsMonadInstanceContextBody(this))
  

   /**
   * If is it statically known, that monad is evaluated in this context, then
   * this call is completely eliminated by dotty-cps-async macro
   *@return fa
   **/
   def adoptAwait[A](fa:F[A]):F[A] = fa
    

}

/**
 * Base trait of CpsContextMonad which provide `Ctx` as a monad context 
 * Mixin this trait into your CosMonad in cases, when you monad have internal API
 * and you potentially want to use moand context as generic type.
 **/
trait CpsContextMonad[F[_],Ctx <: CpsMonadContext[F]]  extends CpsMonad[F] {

  type Context = Ctx

  /**
   * Evaluate operation in context. 
   **/
  def applyContext[T](op: Ctx =>F[T]): F[T]  

  /**
   * delegated to applyContext
   *@see applyContext
   **/
  def apply[T](op: Context => F[T]): F[T] =
    applyContext(c => op(c.asInstanceOf[Context]))
  
}

trait CpsConcurrentContextMonad[F[_], Ctx <: CpsMonadContext[F]] extends CpsConcurrentMonad[F] with CpsContextMonad[F, Ctx] {

  type Context = Ctx

}
