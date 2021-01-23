package cps

import scala.collection.ArrayOps
import scala.collection.IterableOps
import scala.collection.SeqOps
import scala.collection.IndexedSeqOps
import scala.collection.MapOps
import scala.collection.immutable

trait AsyncShift[T]

trait AsyncShiftLowPriority1 {

 transparent inline given shiftedIterable[A,CA <: Iterable[A] ]: AsyncShift[CA] =
      cps.runtime.IterableAsyncShift[A,CA]()

 transparent inline given shiftedRange[CA <: Range]: AsyncShift[CA] =
        cps.runtime.RangeAsyncShift[CA]()

}

trait AsyncShiftLowPriority2 extends AsyncShiftLowPriority1 {

 transparent inline given shiftedIterableOps[A,C[X] <: Iterable[X] & IterableOps[X,C,C[X]] ]: AsyncShift[C[A]] =
      cps.runtime.IterableOpsAsyncShift[A,C,C[A]]()

}

object AsyncShift extends AsyncShiftLowPriority2 {

 transparent inline given shiftedArrayOps[A]: AsyncShift[scala.collection.ArrayOps[A]] =
      new cps.runtime.ArrayOpsAsyncShift[A]()

 transparent inline given shiftedSeqOps[A,C[X] <: Seq[X] & SeqOps[X,C,C[X]] ]: AsyncShift[C[A]] =
      cps.runtime.SeqAsyncShift[A,C,C[A]]()

 transparent inline given shiftedIndexedSeqOps[A,C[X] <: IndexedSeq[X] & IndexedSeqOps[X,C,C[X]] ]: AsyncShift[C[A]] =
      cps.runtime.IndexedSeqAsyncShift[A,C,C[A]]()

 transparent inline given shiftedMapOps[K,V,CC[K,V] <: MapOps[K,V,CC,CC[K,V]] with Iterable[(K,V)]]: AsyncShift[CC[K,V]] =
      cps.runtime.MapOpsAsyncShift[K,V,CC,Iterable,CC[K,V]]()

 transparent inline given shiftedImmutableMapOps[K,V,CC[K,V] <: MapOps[K,V,CC,CC[K,V]] with immutable.Iterable[(K,V)]]: AsyncShift[CC[K,V]] =
      cps.runtime.MapOpsAsyncShift[K,V,CC,immutable.Iterable,CC[K,V]]()

 transparent inline given shiftedList[A]: AsyncShift[scala.collection.immutable.List[A]] =
      cps.runtime.ListAsyncShift[A]()

 transparent inline given shiftedCpsMonad[F[_], M <: CpsMonad[F]](using CpsMonad[F]): AsyncShift[M] = new cps.runtime.CpsMonadSelfAsyncShift[F,M]

 transparent inline given shiftedOption[A]: AsyncShift[Option[A]] =
      new cps.runtime.OptionAsyncShift[A]()

 transparent inline given shiftedFunction1[A,B]: AsyncShift[Function1[A,B]] =
      cps.runtime.Function1AsyncShift[A,B]()

 transparent inline given shiftedPartialFunction[A,B]: AsyncShift[PartialFunction[A,B]] =
      cps.runtime.PartialFunctionAsyncShift[A,B]()

 transparent inline given shiftedTry[A]: AsyncShift[scala.util.Try[A]] =
      new cps.runtime.util.TryAsyncShift[A]()

 transparent inline given shiftedTryModule: AsyncShift[scala.util.Try.type] =
      cps.runtime.util.TryModuleAsyncShift


 transparent inline given shiftedUsing: AsyncShift[scala.util.Using.type] =
       cps.runtime.util.UsingAsyncShift

 transparent inline given shiftedEither[A,B]: AsyncShift[Either[A,B]] =
      cps.runtime.util.EitherAsyncShift[A,B]()

 transparent inline given shiftedEitherLeftProjection[A,B]: AsyncShift[Either.LeftProjection[A,B]] =
      cps.runtime.util.EitherLeftProjectionAsyncShift[A,B]()

}




