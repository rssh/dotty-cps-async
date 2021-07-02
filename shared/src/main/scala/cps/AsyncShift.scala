package cps

import scala.collection.ArrayOps
import scala.collection.IterableOps
import scala.collection.SeqOps
import scala.collection.IndexedSeqOps
import scala.collection.MapOps
import scala.collection.immutable

/**
 * AsynsShift is a marker base trait for typeclass, which provides 'shifted' variants of the hight-order methods 
 * of `T,` which called when we need to pass a cps-transformed function as an argument for this method.
 * 
 *
 * The general convention is next:
 *  - Let us have object `O` and method `m(f: A=>B):R` which accept hight-order argument `f: A=>B.`  
 * (for example - map in List).
 *  - If we want to defined transformation of argument for any monad F, we should define the `AsyncShift[O]`  
 * with method 
 *  ```m[F[_],...](o:O, m:CpsMonad[F])(f: A=>F[B])```.
 *  - Return type of this method can be F[R]  or R or AsyncSubst[R].
 * 
 * Also we should define a given instance of AsyncShift[O], visible from our async block.
 * I.e. implementation for our list will look as:
 *
 * ```
 *     class MyShiftedList[T] extentds AsyncShift[List[T]] {
 *
 *       def map[F[_],S](m:CpsMonad[M], c:List[T])(f: T=>F[S]): F[List[T]] = 
 *           ... // implementation here
 *
 *     }
 *
 *     transparent inline given myShiftedList[T]: AsyncShift[List[T]] = MyShiftedList[T]()
 * ```
 * 
 * After this, you can freely use awaits inside "List.map":
 *
 *```
 *    async {
 *      ....
 *      val fetched = uris.map(uri => await(fetch(uri)))
 *      ...
 *    }
 *```
 * see https://rssh.github.io/dotty-cps-async/HighOrderFunctions.html
 **/
trait AsyncShift[T]

trait AsyncShiftLowPriority1 {

 transparent inline given shiftedIterable[A,CA <: Iterable[A] ]: AsyncShift[CA] =
      cps.runtime.IterableAsyncShift[A,CA]()


}

trait AsyncShiftLowPriority2 extends AsyncShiftLowPriority1 {

 transparent inline given shiftedIterableOps[A,C[X] <: Iterable[X] & IterableOps[X,C,C[X]] ]: AsyncShift[C[A]] =
      cps.runtime.IterableOpsAsyncShift[A,C,C[A]]()

}

object AsyncShift extends AsyncShiftLowPriority2 {

 transparent inline given shiftedRange[CA <: Range]: AsyncShift[CA] =
        cps.runtime.RangeAsyncShift[CA]()

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




