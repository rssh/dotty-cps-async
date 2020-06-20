package cps
 
import scala.collection.ArrayOps
import scala.collection.IterableOps

trait AsyncShift[T]

object AsyncShift {

 transparent inline given shiftedArrayOps[A] as AsyncShift[scala.collection.ArrayOps[A]] = 
      new cps.runtime.ArrayOpsAsyncShift[A]()

 transparent inline given shiftedIterable[A,C[X] <: Iterable[X] & IterableOps[X,C,C[X]] ] as AsyncShift[C[A]] =
      new cps.runtime.IterableAsyncShift[A,C]()

 transparent inline given shiftedList[A] as AsyncShift[scala.collection.immutable.List[A]] =
      new cps.runtime.ListAsyncShift[A]()

 transparent inline given shiftedCpsMonad[F[_], M <: CpsMonad[F]](using CpsMonad[F]) as AsyncShift[M] = new cps.runtime.CpsMonadSelfAsyncShift[F,M]

 transparent inline given shiftedTry[A] as AsyncShift[scala.util.Try[A]] =
      new cps.runtime.TryAsyncShift[A]()

 transparent inline given shiftedOption[A] as AsyncShift[Option[A]] =
      new cps.runtime.OptionAsyncShift[A]()

}

trait AsyncShifted[T,F[_]]

trait ObjectAsyncShift[T]
{
  //def apply[F[_]](obj:T, cpsMonad: CpsMonad[F]): AsyncShifted[T,F]
}


