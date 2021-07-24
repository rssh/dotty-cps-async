package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import java.util.concurrent.atomic.AtomicReference

class ArrayOpsAsyncShift[A] extends AsyncShift[ArrayOps[A]] {

  // TODO: think about default semantics for foreach.
  def foreachParallel[F[_],U](arrayOps: ArrayOps[A], monad: CpsMonad[F])(f: A => F[U]): F[Unit] = {
     val r = new AtomicReference[F[Unit]](monad.pure(()))
     arrayOps.foreach{ a =>
       val b = f(a)
       r.getAndUpdate(currR => monad.flatMap(currR)(_ => monad.map(b)(_ =>())))
     }
     r.get
  }

  def foreachSequential[F[_],U](arrayOps: ArrayOps[A], monad: CpsMonad[F])(f: A => F[U]): F[Unit] =  {
     import cps.syntax.*
     given CpsMonad[F] = monad
     arrayOps.foldLeft[F[Unit]](monad.pure(())){ (s,e) =>
        // use run f(e) only when prev. is evaluated
        s.flatMap(_ => f(e).map(_ =>()))
     }
  }

  def foreach[F[_],U](arrayOps: ArrayOps[A], monad: CpsMonad[F])(f: A => F[U]): F[Unit] =  {
      // TODO: thing about right parallel model
      foreachSequential(arrayOps, monad)(f)  
  }

  def map[F[_]:CpsMonad,B:ClassTag](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[B]):F[Array[B]] = {
     val mappedArr = arr.map(f)
     val r = new Array[B](arr.knownSize)
     val fu = mappedArr.zipWithIndex.foldLeft(monad.pure(())){
       case (s, (fa,i)) => 
           monad.flatMap(s)(_ => monad.map(fa)(b => {r(i) = b}))
     }
     monad.map(fu)(_ => r)
  }

  def flatMap[F[_]:CpsMonad,B:ClassTag](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[Array[B]] = {
     val b = monad.pure(ArrayBuilder.make[B])
     monad.map(
      arr.foldLeft(b)((s,e) => 
       monad.flatMap(s)( sb =>
         monad.map(f(e))(eb => {
            sb ++= eb
            sb
         }))
      )
     )( _.result )
  }

}
