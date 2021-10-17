package cps


/**
 * How this monad can be memoized.
 * 
 * see [chapter in User Guide](https://rssh.github.io/dotty-cps-async/AutomaticColoring.html#automatic-coloring-memoization)
 */
sealed trait CpsMonadMemoization[F[_]]

object CpsMonadMemoization:

  enum Kind:
    case BY_DEFAULT, INPLACE, PURE, DYNAMIC

  trait Inplace[F[_]]  extends CpsMonadMemoization[F]:
    def apply[T](ft:F[T]): F[T]

  object Inplace:

    def run[F[_],E <: F[T], T](mm: Inplace[F], value:E): E =
       mm.apply(value).asInstanceOf[E]
 

  trait Pure[F[_]]  extends CpsMonadMemoization[F]:
    def apply[T](ft:F[T]): F[F[T]]


  trait Dynamic[F[_]] extends CpsMonadMemoization[F]

  trait DynamicAp[F[_],T,FT]:
    def apply(ft:FT):F[FT]

  /**
   * DefaultMemoization means, that our model is already memoized.
   **/
  class Default[F[_]]  extends Inplace[F]:
    def apply[T](ft:F[T]): F[T] = ft


