package cps

import cps.runtime.util.UsingAsyncShift

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

trait AsyncShiftLowPriority0 {

  import cps.runtime.*

  transparent inline given shiftedCpsMonad[F[_], M <: CpsMonad[F]](using CpsMonad[F]): CpsMonadSelfAsyncShift[F, M] =
    new cps.runtime.CpsMonadSelfAsyncShift[F, M]


  transparent inline given shiftedIterable[A, CA <: Iterable[A]]: IterableAsyncShift[A, CA] =
    cps.runtime.IterableAsyncShift[A, CA]()


}

trait AsyncShiftLowPriority1 extends AsyncShiftLowPriority0 {

  import cps.runtime.*

  transparent inline given shiftedIterableOps[A, C[X] <: Iterable[X] & IterableOps[X, C, C[X]]]: IterableOpsAsyncShift[A, C, C[A]] =
    cps.runtime.IterableOpsAsyncShift[A, C, C[A]]()

  transparent inline given shiftedSeqOps[A, C[X] <: Seq[X] & SeqOps[X, C, C[X]]]: SeqAsyncShift[A, C, C[A]] =
    cps.runtime.SeqAsyncShift[A, C, C[A]]()


}

trait AsyncShiftLowPriority2 extends AsyncShiftLowPriority1 {

  import cps.runtime.*


  transparent inline given shiftedMapOps[K, V, CC[K, V] <: MapOps[K, V, CC, CC[K, V]] with Iterable[(K, V)]]: MapOpsAsyncShift[K, V, CC, Iterable, CC[K, V]] =
    MapOpsAsyncShift[K, V, CC, Iterable, CC[K, V]]()


  transparent inline given shiftedIndexedSeqOps[A, C[X] <: IndexedSeq[X] & IndexedSeqOps[X, C, C[X]]]: IndexedSeqAsyncShift[A, C, C[A]] =
    new IndexedSeqAsyncShift[A, C, C[A]]()


}


/**
 *Companion object where defined given AsyncShift instances for Scala standard library objects.
 *
 *@see [cps.AsyncShift] 
 **/
object AsyncShift extends AsyncShiftLowPriority2 {

 import cps.runtime.*
 import cps.runtime.concurrent.*
 import cps.runtime.util.*

 transparent inline given shiftedRange[CA <: Range] : RangeAsyncShift[CA] =
        cps.runtime.RangeAsyncShift[CA]()

 transparent inline given shiftedArrayOps[A]: ArrayOpsAsyncShift[A] =
      new ArrayOpsAsyncShift[A]()



 transparent inline given shiftedImmutableMapOps[K,V,CC[K,V] <: MapOps[K,V,CC,CC[K,V]] with immutable.Iterable[(K,V)]]: MapOpsAsyncShift[K,V,CC,immutable.Iterable,CC[K,V]] =
      MapOpsAsyncShift[K,V,CC,immutable.Iterable,CC[K,V]]()

 //transparent inline given shiftedList[A]: AsyncShift[scala.collection.immutable.List[A]] =
 //     cps.runtime.ListAsyncShift[A]()

 inline given shiftedList[A]: cps.runtime.ListAsyncShift[A] =
      ListAsyncShift[A]()

 transparent inline given shiftedOption[A]: OptionAsyncShift[A] =
      cps.runtime.OptionAsyncShift[A]()

 transparent inline given shiftedFunction1[A,B]: Function1AsyncShift[A,B] =
      Function1AsyncShift[A,B]()

 transparent inline given shiftedPartialFunction[A,B]: PartialFunctionAsyncShift[A,B] =
      PartialFunctionAsyncShift[A,B]()

 transparent inline given shiftedTry[A]: TryAsyncShift[A] =
      TryAsyncShift[A]()

 transparent inline given shiftedTryModule: TryModuleAsyncShift.type =
      TryModuleAsyncShift

 transparent inline given shiftedUsing: UsingAsyncShift.type =
       UsingAsyncShift

 transparent inline given shiftedEither[A,B]: EitherAsyncShift[A,B] =
      EitherAsyncShift[A,B]()

 transparent inline given shiftedEitherLeftProjection[A,B]: EitherLeftProjectionAsyncShift[A,B] =
      cps.runtime.util.EitherLeftProjectionAsyncShift[A,B]()

 transparent inline given shiftedNonLocalReturns: cps.runtime.util.control.NonLocalReturnsAsyncShift.type =
     cps.runtime.util.control.NonLocalReturnsAsyncShift     

 transparent inline given shiftedBoundary: BoundaryAsyncShift.type =
     BoundaryAsyncShift

 transparent inline given shiftedFutureCM: FutureCMAsyncShift =
     FutureCMAsyncShift

}




