package cps

import scala.compiletime.*
import scala.util.NotGiven

trait CpsMonadContext[F[_]] {

  /**
   * adopt monadic value in await to current context.
   **/
  def adoptAwait[A](fa:F[A]):F[A]
 
}


trait CpsMonadInstanceContext[F[_]] extends CpsMonad[F] with CpsMonadContext[F] {

  override type Context = CpsMonadInstanceContext[F]

  /**
  * run with this instance
  **/
  def apply[T](op: Context => F[T]): F[T] =
    op(this.asInstanceOf[Context])

  /**
  *@return fa
  **/
  def adoptAwait[A](fa:F[A]):F[A] = fa

}

trait CpsContextMonad[F[_],Ctx <: CpsMonadContext[F]]  extends CpsMonad[F] {

  override type Context = Ctx

  def applyContext[T](op: Ctx =>F[T]): F[T]  

  def apply[T](op: Context => F[T]): F[T] =
    applyContext(c => op(c.asInstanceOf[Context]))
  
}
