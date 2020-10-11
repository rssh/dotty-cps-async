package cps.runtime

import cps._
import scala.collection._

class MapOpsAsyncShift[K,V,+CC[KX,VX] <: MapOps[KX,VX,CC,CC[KX,VX]] with CI[(KX,VX)], 
                            CI[X] <: Iterable[X] & IterableOps[X,CI,CI[X]],
                                                                 CKV <: CC[K,V] with PartialFunction[K,V] ] extends
                                                                       IterableOpsAsyncShift[(K,V),CI,CKV]
                                                                      with
                                                                       PartialFunctionAsyncShiftBase[K,V, CKV]
                                                                      with
                                                                       AsyncShift[CKV]:

 //def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2]

 //defforeachEntry[U](f: (K, V) => U): Unit

 def getOrElse[F[_],V1 >: V](c: CKV, m: CpsMonad[F])(key: K, default: () => F[V1]): F[V1] =
   c.get(key) match
     case Some(v) => m.pure(v)
     case None => default()
   


    

