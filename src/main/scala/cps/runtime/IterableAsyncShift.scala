package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

class IterableAsyncShift[A, CA <: Iterable[A] ] extends AsyncShift[CA] {

  def shiftedFold[F[_],S,B,R](c:CA, monad:CpsMonad[F])(
                                      prolog:S, 
                                      action: A=>F[B],
                                      acc: (S,A,B)=> S, 
                                      epilog:S=>R):F[R] =
   val r = c.foldLeft(monad.pure(prolog)){(ms,a) =>
      val mb = action(a)
      monad.flatMap(ms){ s=>
        monad.map(mb){ b=> acc(s,a,b) }
      }
   }
   monad.map(r)(epilog)

  def shiftedStateFold[F[_],S,R](c:CA, monad: CpsMonad[F])(
                            prolog: S,
                            acc: (S,A) => F[S],
                            epilog:S=>R):F[R] =
   val r = c.foldLeft(monad.pure(prolog)){(ms,a) =>
                monad.flatMap(ms){ s=> acc(s,a) }
           }
   monad.map(r)(epilog)
    
  def shiftedWhile[F[_],S,R](c:CA,monad:CpsMonad[F])(
                                      prolog:S, 
                                      condition: A=>F[Boolean],
                                      acc: (S,Boolean,A)=>S,
                                      epilog: S =>R):F[R] =
     def checkIt(it:Iterator[A],s:S):F[S] = 
        if !it.hasNext then
           monad.pure(s)
        else 
           val a = it.next
           monad.flatMap(condition(a)){c =>
             if (c) 
               checkIt(it,acc(s,true,a)) 
             else 
               monad.pure(acc(s,false,a))
           }
     val r = checkIt(c.iterator,prolog)
     monad.map(r)(epilog)
     
     
  def foreach[F[_],U](c: CA, monad: CpsMonad[F])(f: A => F[U]): F[Unit] = 
     shiftedFold(c,monad)((),f,(s,a,b)=>s,identity)

  def count[F[_]](c:CA, monad: CpsMonad[F])(p: A => F[Boolean]):F[Int] = 
    shiftedFold(c,monad)(0,p,(s,a,b)=>if (b) s+1 else s,identity)

  def exists[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[Boolean] =
    shiftedWhile(c,monad)(false, 
             x => monad.map(p(x))(! _),
             (s,c,a) => !c, identity)


  def find[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[Option[A]] =
    val s0: Option[A] = None
    shiftedWhile(c,monad)(s0, 
                        a => monad.map(p(a))(! _),
                        (state,notFound,a)=> if (notFound) state else Some(a),
                        identity
                        ) 

  def fold[F[_],A1 >: A](c:CA,monad: CpsMonad[F])(z:A1)(op:(A1,A1)=>F[A1]):F[A1]=
    shiftedStateFold(c,monad)(z, op, identity)
    

}

class IterableOpsAsyncShift[A, C[X] <: Iterable[X] & IterableOps[X,C,C[X]] ] 
                                                        extends IterableAsyncShift[A,C[A]] {



  def map[F[_], B](c: C[A], monad: CpsMonad[F])(f: A=> F[B]):F[C[B]] = 
    shiftedFold(c,monad)(
      c.iterableFactory.newBuilder[B], 
      f,
      (s,a,b) => s.addOne(b),
      _.result
    )
    
  def flatMap[F[_], B](c: C[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[C[B]] = 
    shiftedFold(c,monad)(
       c.iterableFactory.newBuilder[B],
       f,
       (s,a,b) => s.addAll(b),
       _.result
    )


  def dropWhile[F[_]](c:C[A], monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    shiftedWhile(c,monad)(c,p,(s,c,a)=>if (c) s.drop(1) else s,identity)
    

  def filter[F[_]](c:C[A], monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    shiftedFold(c,monad)(
       c.iterableFactory.newBuilder[A],p,
       (s,a,b) => if (b) s.addOne(a) else s,
       _.result
    )

  def filterNot[F[_]](c:C[A], monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    filter(c,monad)(a => monad.map(p(a))(! _))


  def flatten[F[_],B](c:C[A],monad: CpsMonad[F])(implicit asIterable: (A)=> F[IterableOnce[B]]):F[C[B]]=
    shiftedFold(c,monad)(
       c.iterableFactory.newBuilder[B],
       asIterable,
       (s,a,b) => s.addAll(b),
       _.result    
    )
                          

}


