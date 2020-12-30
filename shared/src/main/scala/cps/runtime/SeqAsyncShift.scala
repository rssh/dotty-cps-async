package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

class SeqAsyncShift[A, C[X] <: Seq[X] & SeqOps[X,C,C[X]], CA <: C[A]] extends IterableOpsAsyncShift[A,C,CA] {

  // TODO: move to IndexedSeq
  def aggregate[F[_], B](c:C[A], m: CpsMonad[F])(
        z: () => F[B])(seqop: (B, A) => F[B], combop: (B, B) ⇒ F[B]): F[B] =
     c.aggregate[F[B]](z())(
            (fb, a) => m.flatMap(fb)(b=>seqop(b,a)),
            (fbx, fby) => m.flatMap(fbx)(bx=>m.flatMap(fby)(by=>combop(bx,by)))
     )
                      
  def distinctBy[F[_],B](c:CA, m: CpsMonad[F])(f: (A)=>F[B]): F[C[A]] =
     shiftedFold(c,m)(
       (immutable.Set.empty[B], c.iterableFactory.newBuilder[A]),
       f,
       (s,a,b) => { val (olds, it) = s
         if (olds.contains(b)) then
           s
         else
           (olds.incl(b), it.addOne(a))
       },
       _._2.result
     )

}


class SeqOpsAsyncShift[A, C[X] <: Seq[X] & SeqOps[X,C,C[X]], CA <: C[A] ] extends IterableOpsAsyncShift[A,C, CA] {


}

class RangeAsyncShift[ R <: Range ] extends SeqAsyncShift[Int,immutable.IndexedSeq,R]
