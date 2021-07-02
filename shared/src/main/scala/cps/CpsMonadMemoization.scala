package cps


enum MonadMemoizationKind:
  case BY_DEFAULT, INPLACE, PURE, DYNAMIC

sealed trait CpsMonadMemoization[F[_]]

trait CpsMonadInplaceMemoization[F[_]]  extends CpsMonadMemoization[F]:
  def apply[T](ft:F[T]): F[T]

object CpsMonadInplaceMemoization:

  def run[F[_],E <: F[T], T](mm: CpsMonadInplaceMemoization[F], value:E): E =
       mm.apply(value).asInstanceOf[E]
 

trait CpsMonadPureMemoization[F[_]]  extends CpsMonadMemoization[F]:
  def apply[T](ft:F[T]): F[F[T]]


trait CpsMonadDynamicMemoization[F[_]] extends CpsMonadMemoization[F]


class CpsMonadDefaultMemoization[F[_]]  extends CpsMonadInplaceMemoization[F]:
  def apply[T](ft:F[T]): F[T] = ft


trait CpsMonadDynamicMemoizationAp[F[_],T,FT]:
  def apply(ft:FT):F[FT]


