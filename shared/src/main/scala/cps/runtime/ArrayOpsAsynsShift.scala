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
       r.getAndUpdate(currR => monad.flatMap(currR.nn)(_ => monad.map(b)(_ =>())))
     }
     r.get.nn
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

  def flatMapIterableOnce[F[_],B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]])(using ct:ClassTag[B]):F[Array[B]] = {
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


  def flatMap[F[_],B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]])(using ct:ClassTag[B]):F[Array[B]] = {
     flatMapIterableOnce(arr,monad)(f)
  }

  // note, that implicit parameter also shpuld be transformed.
  def flatMap[F[_],BS,B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=> F[BS])(using asIterableF:BS=>F[Iterable[B]], ct:ClassTag[B]):F[Array[B]] = {     
   def f1(a:A): F[IterableOnce[B]] =
      monad.flatMap(f(a)){ bs =>
         monad.map(asIterableF(bs))(x => x.asInstanceOf[IterableOnce[B]])   
      }
   this.flatMapIterableOnce(arr,monad)(f1)
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
     
  def distinctBy[F[_],B](arr: ArrayOps[A], monad: CpsMonad[F])(f: A=>F[B]): F[Array[A]] = {
      val s0: F[(Set[B],ArrayBuffer[A])] = monad.pure((Set.empty[B], ArrayBuffer[A]()))
      val fr = arr.foldLeft(s0){ (fs,e) =>
         monad.flatMap(fs){ s =>
           monad.map(f(e)){ b =>
              if ! s._1.contains(b) then {
                 s._2.addOne(e)
                 (s._1 + b, s._2)
              } else s
           }
         }
      }
      monad.map(fr){ case (set,buffer) =>
         val r = arr.take(buffer.length)
         buffer.copyToArray(r)
         r
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

  def foldLeft[F[_],B](arrOps: ArrayOps[A], monad: CpsMonad[F])(z: B)(op:(B,A)=>F[B]):F[B] = {
     arrOps.foldLeft[F[B]](monad.pure(z)){ (s,e) =>
        monad.flatMap(s)(b=>op(b,e))
     }
  }

  def foldRight[F[_],B](arrOps: ArrayOps[A], monad: CpsMonad[F])(z: B)(op:(A,B)=>F[B]):F[B] = {
     arrOps.foldRight[F[B]](monad.pure(z)){ (e,s) =>
        monad.flatMap(s)(b=>op(e,b))
     }
  }

  def forall[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p:A => F[Boolean]):F[Boolean] = {
     def step(it: Iterator[A]):F[Boolean] =
       if !it.hasNext then
          monad.pure(true)
       else
          monad.flatMap(p(it.next)){ r =>
             if (r) then step(it) else monad.pure(false)
          }
     step(arrOps.iterator)
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
             v.copyToArray(r,0,l)
             r
          }.toMap
        }
  }

  def groupMap[F[_],K,B](arrOps: ArrayOps[A], monad: CpsMonad[F])(key: A=> F[K])(f: A=>F[B])(using ClassTag[B]): F[Map[K,Array[B]]] = {
        def step(it: Iterator[A], gathered: Map[K,ArrayBuilder[B]]):F[Map[K,ArrayBuilder[B]]] =
          if !it.hasNext then
             monad.pure(gathered)
          else
             val e = it.next
             monad.flatMap(key(e)){ k=>
               monad.flatMap(f(e)){ b =>
                 gathered.get(k) match
                   case Some(v) => v.addOne(b)
                                   step(it,gathered)
                   case None =>
                       step(it, gathered.updated(k,ArrayBuilder.make[B].addOne(b)))
               }
             }
        val s = step(arrOps.iterator, Map.empty) 
        monad.map(s){ (gathered: Map[K,ArrayBuilder[B]]) =>
          gathered.view.mapValues(_.result).toMap
        }
  }

  def indexWhere[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean], from: Int): F[Int] = {
     def steps(it: Iterator[A], i:Int):F[Int] =
       if !it.hasNext then
          monad.pure(-1)
       else
          val e = it.next
          monad.flatMap(p(e)){ v =>
             if (v) then monad.pure(i) else steps(it, i+1)
          }
     val startView = arrOps.drop(from)
     steps(startView.iterator, from)
  }

  def lastIndexWhere[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean], end: Int): F[Int] = {
     def steps(view: IndexedSeqView[A], i:Int): F[Int] =
       if (i < 0) then
          monad.pure(-1)     
       else
          val e = view(i)
          monad.flatMap(p(e)){ v =>
             if (v) then monad.pure(i) else steps(view, i-1)
          }
     steps(arrOps.view, end) 
  }

  def partition[F[_]](arrOps: ArrayOps[A], monad: CpsMonad[F])(p: A=> F[Boolean]): F[(Array[A],Array[A])] = {
      def steps(it: Iterator[A], b1: ArrayBuffer[A], b2: ArrayBuffer[A]): F[(ArrayBuffer[A],ArrayBuffer[A])] = {
         if !it.hasNext then
            monad.pure((b1,b2))
         else
            val e = it.next
            monad.flatMap(p(e)){ v =>
               if (v) then b1.addOne(e) else b2.addOne(e)
               steps(it,b1,b2)
            }
      }
      val s = steps(arrOps.iterator, new ArrayBuffer[A], new ArrayBuffer[A])
      monad.map(s){ (b1,b2) =>
        val r1 = arrOps.slice(0,b1.size)
        b1.copyToArray(r1)
        val r2 = arrOps.slice(0,b2.size)
        b2.copyToArray(r2)
        (r1,r2)
      }
  }

  def partitionMap[F[_], A1, A2](arrOps: ArrayOps[A], monad: CpsMonad[F])(f: A => F[Either[A1,A2]])(using ClassTag[A1], ClassTag[A2]): F[(Array[A1],Array[A2])] = {
      def steps(it: Iterator[A], b1: ArrayBuilder[A1], b2: ArrayBuilder[A2]): F[(ArrayBuilder[A1],ArrayBuilder[A2])] = {
         if !it.hasNext then
            monad.pure((b1,b2))
         else
            val e = it.next
            monad.flatMap(f(e)){ v =>
               v match
                  case Left(l) => b1.addOne(l)
                  case Right(r) => b2.addOne(r)
               steps(it,b1,b2)
            }
      }
      val s = steps(arrOps.iterator, ArrayBuilder.make[A1], ArrayBuilder.make[A2])
      monad.map(s){ (b1,b2) =>
        (b1.result, b2.result)
      }
  }

  def scanLeft[F[_], B](arrOps: ArrayOps[A], monad: CpsMonad[F])(z:B)(op: (B,A) => F[B])(using ClassTag[B]): F[Array[B]] = {
      val trace0 = ArrayBuilder.make[B]
      val r = arrOps.foldLeft(monad.pure((trace0,z))){ (b, a) =>
         monad.flatMap(b){ case (trace, state) =>
            monad.map(op(state, a)){ next =>
               trace.addOne(state)
               (trace, next)
            }
         }
      }
      monad.map(r){ case (trace,next) =>
         trace.addOne(next)
         trace.result
      }
  }
 
  def scanRight[F[_], B](arrOps: ArrayOps[A], monad: CpsMonad[F])(z:B)(op: (A,B) => F[B])(using ClassTag[B]): F[Array[B]] = {
      val trace0 = ArrayBuilder.make[B]
      val r = arrOps.foldRight(monad.pure((trace0,z))){ (a, b) =>
         monad.flatMap(b){ case (trace, state) =>
            monad.map(op(a, state)){ next =>
               trace.addOne(state)
               (trace, next)
            }
         }
      }
      monad.map(r){ case (trace,next) =>
         trace.addOne(next)
         trace.result.reverse
      }
  }

  def span[F[_]](arrayOps: ArrayOps[A], monad: CpsMonad[F])(p: A => F[Boolean]): F[(Array[A],Array[A])] = {
      val fb = indexWhere(arrayOps,monad)(x => monad.map(p(x))(!_),0)
      monad.map(fb){ b => 
         if (b == -1) {
            (arrayOps.slice(0,arrayOps.size),arrayOps.slice(0,0))
         } else {
            (arrayOps.slice(0,b),arrayOps.slice(b,arrayOps.size))
         }
      } 
  }

  def takeWhile[F[_]](arrayOps: ArrayOps[A], monad: CpsMonad[F])(p: A => F[Boolean]): F[Array[A]] = {
      val fb = indexWhere(arrayOps,monad)(x => monad.map(p(x))(!_),0)
      monad.map(fb){ b => 
         if (b == -1) {
            arrayOps.slice(0,arrayOps.size)
         } else {
            arrayOps.slice(0,b)
         }
      } 
  }

  def withFilter[F[_]](arrayOps: ArrayOps[A], monad: CpsMonad[F])(p: A => F[Boolean]):ArrayOpsWithFilterAsyncSubst[F,A] =
   new ArrayOpsWithFilterAsyncSubst(arrayOps, monad, p)

}

class ArrayOpsWithFilterAsyncSubst[F[_],A](ops: ArrayOps[A], monad:CpsMonad[F], p: A=>F[Boolean]) extends CallChainAsyncShiftSubst[F, ArrayOps.WithFilter[A], F[ArrayOps.WithFilter[A]] ]
{
   override def _finishChain = monad.map((new ArrayOpsAsyncShift[A]).filter(ops,monad)(p)){ array =>
      array.withFilter(_ => true)
   }

   def runBuild[B:ClassTag](it: Iterator[A], builder:ArrayBuilder[B])(step: (A,ArrayBuilder[B]) => F[Unit]): F[ArrayBuilder[B]] =
      if (it.hasNext) then
         val e = it.next
         monad.flatMap(p(e)){ v =>
            if (v) then
               monad.flatMap(step(e,builder)){ u =>
                  runBuild(it, builder)(step)
               }
            else
               runBuild(it, builder)(step) 
         }
      else
         monad.pure(builder)

   def startBuild[B:ClassTag](step: (A,ArrayBuilder[B])=>F[Unit]): F[Array[B]] =
      val fb = runBuild(ops.iterator, ArrayBuilder.make[B])(step)
      monad.map(fb)(_.result)

   def flatMap[B:ClassTag](f: A=>IterableOnce[B]): F[Array[B]] =
         startBuild{ (a,builder) =>
            val itb = f(a).iterator
            while(itb.hasNext) {
              builder.addOne(itb.next)
            }
            monad.pure(())
         }

   def flatMap_async[B:ClassTag](f: A=>F[IterableOnce[B]]): F[Array[B]] =
         startBuild{ (a, builder) =>
            val fbs = f(a)
            monad.map(fbs){ bs =>
               val itb = bs.iterator
               while(itb.hasNext) {
                  builder.addOne(itb.next)
               }
            }
         }
   
         
   def flatMap[BS,B](f: A=>BS)(using asIterable: BS=>Iterable[B], m: ClassTag[B]):F[Array[B]] =
      flatMap(a => asIterable(f(a)))

   def map[B:ClassTag](f: A=>B):F[Array[B]] =
      startBuild{ (a, builder) =>
         builder.addOne(f(a))
         monad.pure(())
      }

   def map_async[B:ClassTag](f: A=>F[B]):F[Array[B]] =
      startBuild{ (a, builder) =>
         monad.map(f(a)){ b=>
            builder.addOne(b)
         }
      }
      
   def withFilter(q: A=>Boolean): ArrayOpsWithFilterAsyncSubst[F,A] =
      new ArrayOpsWithFilterAsyncSubst(ops, monad, {
         a => monad.map(p(a))(  c => if (c) then q(a) else false )
      })

   def withFilter_async(q: A=>F[Boolean]): ArrayOpsWithFilterAsyncSubst[F,A] =
         new ArrayOpsWithFilterAsyncSubst(ops, monad, {
            a => monad.flatMap(p(a))(  c => if (c) then q(a) else monad.pure(false) )
         })
   

}