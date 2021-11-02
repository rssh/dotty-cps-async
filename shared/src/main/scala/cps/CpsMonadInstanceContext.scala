package cps


/**
 * trait, which should be mixed in CpsMoand
 * to provide instance of CpsMonad as context.
 **/ 
trait CpsMonadInstanceContext[F[_]]  {

    this: CpsMonad[F] =>

    override type Context = this.type

    def apply[T](f: Context => F[T]): F[T] =
      f(this)

    def adoptAwait[A](c:Context, fa:F[A]):F[A] =
      fa
}  

