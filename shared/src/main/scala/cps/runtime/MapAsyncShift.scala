package cps.runtime

import cps._
import scala.collection._

class MapOpsAsyncShift[K,V, CC[KX,VX] <: MapOps[KX,VX,CC,CC[KX,VX]] with CI[(KX,VX)], 
                            CI[X] <: Iterable[X] & IterableOps[X,CI,CI[X]],
                                                                 CKV <: CC[K,V] with PartialFunction[K,V] ] extends
                                                                       IterableOpsAsyncShift[(K,V),CI,CKV]
                                                                      with PartialFunctionAsyncShiftBase[K,V, CKV]
                                                                      with AsyncShift[CKV]:
                                                                      

 def flatMap[F[_], K2, V2](c: CKV, m: CpsMonad[F])(f: ((K, V)) => F[IterableOnce[(K2, V2)]]): F[CC[K2, V2]] =
   val s0 = m.pure(c.mapFactory.newBuilder[K2,V2])
   val it = c.foldLeft(s0){ (s,e) =>
      m.flatMap(s){ cc =>
         m.map(f(e._1,e._2)){ vs =>
           cc.addAll(vs)
         }
      }
   }
   m.map(it)(_.result)
   
 def map[F[_], K2, V2](c: CKV, m: CpsMonad[F])(f: ((K, V)) => F[(K2, V2)]): F[CC[K2, V2]] =
   val s0 = m.pure(c.mapFactory.newBuilder[K2,V2])
   val it = c.foldLeft(s0){ (s,e) =>
      m.flatMap(s){ cc =>
         m.map(f(e._1,e._2)){ vs =>
           cc.addOne(vs)
         }
      }
   }
   m.map(it)(_.result)


 def foreachEntry[F[_],U](c: CKV, m:CpsMonad[F])(f: (K, V) => F[U]): F[Unit] =
   foreach[F,U](c, m)(x => f(x._1, x._2) )

 def getOrElse[F[_],V1 >: V](c: CKV, m: CpsMonad[F])(key: K, default: () => F[V1]): F[V1] =
   c.get(key) match
     case Some(v) => m.pure(v)
     case None => default()
   


    

