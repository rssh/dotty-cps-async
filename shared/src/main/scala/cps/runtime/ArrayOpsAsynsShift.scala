package cps.runtime

import cps._
import scala.annotation.targetName
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Map
import java.util.concurrent.atomic.AtomicReference

class ArrayOpsAsyncShift[A] extends AsyncShift[ArrayOps[A]] {

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

  def map[F[_],B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[B])(using ct:ClassTag[B]):F[Array[B]] = {
     val mappedArr = arr.map(f)
     val r = new Array[B](arr.knownSize)
     val fu = mappedArr.zipWithIndex.foldLeft(monad.pure(())){
       case (s, (fa,i)) => 
           monad.flatMap(s)(_ => monad.map(fa)(b => {r(i) = b}))
     }
     monad.map(fu)(_ => r)
  }

  def flatMap[F[_],B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]])(using ct:ClassTag[B]):F[Array[B]] = {
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

  def flatMap[F[_],BS,B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[BS])(using asIterable:BS=>Iterable[B], ct:ClassTag[B]):F[Array[B]] = {
     flatMap(arr,monad)(a => monad.map(f(a))(bs => asIterable(bs)))
  }

  def fold[F[_],A1 >: A](arrOps: ArrayOps[A], monad: CpsMonad[F])(z: A1)(op:(A1,A1)=>F[A1]):F[A1] = {
     arrOps.foldLeft[F[A1]](monad.pure(z)){ (s,e) =>
        monad.flatMap(s){ s1 => op(s1,e) }
     }
  }


  def collectFirst[F[_],B](arrOps: ArrayOps[A], monad: CpsMonad[F])(f: PartialFunction[A,F[B]]): F[Option[B]] = {
     arrOps.collectFirst(f) match
        case Some(fb) => monad.map(fb)(x => Some(x))
        case None => monad.pure(None)
  }

  def collect[F[_],B](arrOps: ArrayOps[A], monad: CpsMonad[F])(f: PartialFunction[A,F[B]])(using ClassTag[B]): F[Array[B]] = {
     val build = arrOps.collect(f).foldLeft(monad.pure(ArrayBuilder.make[B])){ (s,e) =>
         monad.flatMap(s){ s1 =>
          monad.map(e){ e1 =>
           s1.addOne(e1)
           s1
         }
        } 
     }
     monad.map(build)(_.result)
  }


  def count[F[_]](arr: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Int] = {
     arr.foldLeft[F[Int]](monad.pure(0)){ (s,e) =>
        monad.flatMap(s){v =>
          monad.map(p(e)){ x =>
            if x then v+1 else v
          }
        }
     }
  }

  def dropWhile[F[_]](arr: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Array[A]] = {
     def skipWhile(it: Iterator[A], i:Int):F[Array[A]] =
       if !it.hasNext then
          monad.pure(arr.slice(0,1))
       else
          val e = it.next
          monad.flatMap(p(e)){ v =>
             if (v) then skipWhile(it,i+1) else monad.pure(arr.slice(i,arr.size))
          }
     skipWhile(arr.iterator,0)
  }

  def exists[F[_]](arr: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Boolean] = {
     def steps(it: Iterator[A]):F[Boolean] =
       if !it.hasNext then
          monad.pure(false)
       else
          val e = it.next
          monad.flatMap(p(e)){ v =>
             if (v) then monad.pure(true) else steps(it)
          }
     steps(arr.iterator)
  }

  def find[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Option[A]] = {
     def steps(it: Iterator[A]):F[Option[A]] =
       if !it.hasNext then
          monad.pure(None)
       else
          val e = it.next
          monad.flatMap(p(e)){ v =>
             if (v) then monad.pure(Some(e)) else steps(it)
          }
     steps(arrOps.iterator)
  }

  def filter[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Array[A]] = {
     val tmp = arrOps.slice(0,arrOps.size)
     def steps(it: Iterator[A], i:Int):F[Array[A]] =
       if !it.hasNext then
          monad.pure(tmp.slice(0,i))
       else
          val e = it.next
          monad.flatMap(p(e)){ v =>
             if v then 
               tmp(i) = e
               steps(it,i+1)
             else
               steps(it,i)
          }
     steps(arrOps.iterator, 0)
  }

  def filterNot[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[Array[A]] = {
        filter[F](arrOps, monad)(a => monad.map(p(a))(! _))
  }

  def groupBy[F[_],K](arrOps: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[K]): F[Map[K,Array[A]]] = {
        def step(it: Iterator[A], gathered: Map[K,ArrayBuffer[A]]):F[Map[K,ArrayBuffer[A]]] =
          if !it.hasNext then
             monad.pure(gathered)
          else
             val e = it.next
             monad.flatMap(f(e)){ k =>
               gathered.get(k) match
                 case Some(r) => r.addOne(e)
                                 step(it, gathered)
                 case None =>
                    val r = ArrayBuffer(e)
                    step(it, gathered.updated(k,r))
             }
        val s: F[Map[K,ArrayBuffer[A]]] = step(arrOps.iterator, Map.empty) 
        monad.map(s){ (gathered: Map[K,ArrayBuffer[A]]) =>
          gathered.view.mapValues{ v =>
             val l = v.length
             // no element ClassTag, for using toArray
             val r = arrOps.slice(0,l)
             var i = 0
             while(i < l) {
               r(i) = v(i) 
               i = i + 1
             }
             r
          }.toMap
        }
  }


}
