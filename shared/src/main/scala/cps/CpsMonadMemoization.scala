package cps

sealed trait CpsMonadMemoization[F[_]]

trait CpsMonadInplaceMemoization[F[_]]  extends CpsMonadMemoization[F]:
  def apply[T](ft:F[T]): F[T]

object CpsMonadInplaceMemoization:

  def run[F[_],E <: F[T], T](mm: CpsMonadInplaceMemoization[F], value:E): E =
       mm.apply(value).asInstanceOf[E]
 


trait CpsMonadPureMemoization[F[_]]  extends CpsMonadMemoization[F]:
  def apply[T](ft:F[T]): F[F[T]]

class CpsMonadDefaultMemoization[F[_]]  extends CpsMonadInplaceMemoization[F]:
  def apply[T](ft:F[T]): F[T] = ft



