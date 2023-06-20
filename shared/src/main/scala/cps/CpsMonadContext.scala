package cps

import scala.annotation.experimental
import scala.compiletime.*
import scala.util.NotGiven



/**
 * Base for context operations inside monad
 **/
trait CpsMonadContext[F[_]] {

  type Monad[X] = F[X]

  @experimental
  type Direct = CpsDirect[F]


  /**
   *@return instance of cps-monad, where operations can be intercepted by context.
   *   I.e. for context-full operation, like setting common deadline, this deadline can be propagated via
   *   flatMap chains.
   */
   def monad: CpsMonad[F]


  /**
   * adopt external monadic value to the current context.
   **/
  //@deprecated("use wrapped monad operation instead", "0.17")
  //def adoptAwait[A](fa:F[A]):F[A]
 

}

trait CpsThrowMonadContext[F[_]] extends CpsMonadContext[F] {

  /**
   * @return instance of cps-monad which should supports throw operations.
   */
  override def monad: CpsThrowMonad[F]

}

trait CpsTryMonadContext[F[_]] extends CpsThrowMonadContext[F] {

  /**
   * @return instance of cps-monad which should supports try operations.
   */
  override def monad: CpsTryMonad[F]


}

trait CpsConcurrentMonadContext[F[_]] extends CpsTryMonadContext[F] {

  /**
   * @return instance of cps-monad which should supports concurrent operations.
   */
  override def monad: CpsConcurrentMonad[F]

}

object CpsMonadContext {

  @experimental
  given monadContext[F[_]](using direct:CpsDirect[F]): CpsTryMonadContext[F] =
     direct.context

}




class CpsPureMonadInstanceContextBody[F[_]](m: CpsPureMonadInstanceContext[F]) extends CpsMonadContext[F] {


  /**
   * @return return instance of cps-moand where operations can be intercepred by context.
   */
  def monad: CpsMonad[F] = m

}


/**
 * Trait for minimal monad context, which provides an instance of CpsMonad.
 * Mixin this trait into your monad in cases, when you monad have no internal API and
 * not support try/catch operations.
 **/
trait CpsPureMonadInstanceContext[F[_]] extends CpsMonad[F] {


  type Context = CpsPureMonadInstanceContextBody[F]

  /**
  * run with this instance
  **/
  def apply[T](op: Context => F[T]): F[T] =
    op(CpsPureMonadInstanceContextBody(this))


   ///**
   //* If is it statically known, that monad is evaluated in this context, then
   //* this call is completely eliminated by dotty-cps-async macro
   //*@return fa
   //**/
   //def adoptAwait[A](fa:F[A]):F[A] = fa


}

class CpsThrowMonadInstanceContextBody[F[_]](val m: CpsThrowMonadInstanceContext[F]) extends CpsThrowMonadContext[F]  {

    override def monad: CpsThrowMonad[F] = m

}

/**
 * Minimal monad context, which provides an instance of CpsThrowMonad.
 * Use it if your monad supports throw operation but not try/catch.
 * @tparam F
 */
trait CpsThrowMonadInstanceContext[F[_]] extends CpsThrowMonad[F] {

    override type Context = CpsThrowMonadInstanceContextBody[F]

    override def apply[T](op: Context => F[T]): F[T] =
      op(CpsThrowMonadInstanceContextBody(this))

}

class CpsTryMonadInstanceContextBody[F[_]](val m: CpsTryMonadInstanceContext[F]) extends CpsTryMonadContext[F]  {

    override def monad: CpsTryMonad[F] = m

}


/**
 * Minimal monad context, which provides an instance of CpsTryMonad.
 * Use it if your monad supports throw and try/catch operations.
 * @tparam F
 */
trait CpsTryMonadInstanceContext[F[_]] extends CpsTryMonad[F] {

    override type Context = CpsTryMonadInstanceContextBody[F]

    override def apply[T](op: Context => F[T]): F[T] =
      op(CpsTryMonadInstanceContextBody(this))

}


/**
 * @Deprecated  Use instead one of
 * - CpsTryMonadInstanceContext  for monads supporting trow and try/catch
 * - CpsThrowMonadInstanceContext for monads supporting only throw
 * - CpsPureMonadInstanceContext for monads which not support throw and try/catch
 */
@deprecated("use CpsTryMonadInstanceContext, CpsThrowMonadInstanceContext or CpsPureMonadInstanceContext instead", "0.17")
trait CpsMonadInstanceContext[F[_]] extends CpsTryMonadInstanceContext[F]

@deprecated("use CpsTryMonadInstanceContextBody, CpsThrowMonadInstanceContext or CpsPureMonadInstanceContext instead", "0.17")
type CpsMonadInstanceContextBody[F[_]] = CpsTryMonadInstanceContextBody[F]

trait CpsAsyncMonadInstanceContext[F[_]] extends CpsAsyncMonad[F] with CpsTryMonadInstanceContext[F]

trait CpsAsyncEffectMonadInstanceContext[F[_]] extends CpsAsyncEffectMonad[F] with CpsAsyncMonadInstanceContext[F]

trait CpsConcurrentMonadInstanceContext[F[_]] extends CpsConcurrentMonad[F] with CpsAsyncMonadInstanceContext[F]

trait CpsConcurrentEffectMonadInstanceContext[F[_]] extends CpsConcurrentEffectMonad[F]
                                                       with CpsConcurrentMonadInstanceContext[F]
                                                       with CpsAsyncEffectMonadInstanceContext[F] {

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

trait CpsTryContextMonad[F[_],Ctx <: CpsTryMonadContext[F]] extends CpsContextMonad[F, Ctx] with CpsTryMonad[F] {

  override type Context = Ctx

  override def apply[T](op: Context => F[T]): F[T] =
    applyContext(c => op(c.asInstanceOf[Context]))

}


trait CpsConcurrentContextMonad[F[_], Ctx <: CpsTryMonadContext[F]] extends CpsConcurrentMonad[F] with CpsTryContextMonad[F, Ctx] {

  type Context = Ctx

}


