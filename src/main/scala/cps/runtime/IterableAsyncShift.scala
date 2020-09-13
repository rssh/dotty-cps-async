package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

class IterableAsyncShift[A, CA <: Iterable[A] ] extends AsyncShift[CA] {

  /**
   * sequentially do action. each action is started after prev. is finished
   */
  def shiftedFold[F[_],S,B,R](c:CA, monad:CpsMonad[F])(
                                      prolog:S, 
                                      action: A=>F[B],
                                      acc: (S,A,B)=> S, 
                                      epilog:S=>R):F[R] =
   val r = c.foldLeft(monad.pure(prolog)){(ms,a) =>
      monad.flatMap(ms){ s=>
        val mb = action(a)
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
     
  def collectFirst[F[_],B](c:CA, monad: CpsMonad[F])(pf: PartialFunction[A,F[B]]): F[Option[B]] =
       c.collectFirst(pf) match
          case Some(fb) => monad.map(fb)(Some(_))
          case None => monad.pure(None)
     
  def foreach[F[_],U](c: CA, monad: CpsMonad[F])(f: A => F[U]): F[Unit] = 
     shiftedFold(c,monad)((),f,(s,a,b)=>s,identity)

  def corresponds[F[_],B](c:CA, monad: CpsMonad[F])(that: IterableOnce[B])(p: (A,B)=>F[Boolean]) =
     def checkNext(itA:Iterator[A],itB:Iterator[B]):F[Boolean] = 
          if itA.hasNext then
            if itB.hasNext then
              monad.flatMap(p(itA.next,itB.next)){ c =>
                if (c) 
                   checkNext(itA,itB)
                else
                   monad.pure(false)
              }
            else
              monad.pure(false)
          else
            monad.pure(!itB.hasNext)
     checkNext(c.iterator, that.iterator)


  def count[F[_]](c:CA, monad: CpsMonad[F])(p: A => F[Boolean]):F[Int] = 
    shiftedFold(c,monad)(0,p,(s,a,b)=>if (b) s+1 else s,identity)

  def exists[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[Boolean] =
    shiftedWhile(c,monad)(false, 
             x => monad.map(p(x))(! _),
             (s,c,a) => !c, identity)

  def forall[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[Boolean] =
    shiftedWhile(c,monad)(true, p, (s,c,a) => c, identity) 
                          
  def find[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[Option[A]] =
    val s0: Option[A] = None
    shiftedWhile(c,monad)(s0, 
                        a => monad.map(p(a))(! _),
                        (state,notFound,a)=> if (notFound) state else Some(a),
                        identity
                        ) 

  def fold[F[_],A1 >: A](c:CA,monad: CpsMonad[F])(z:A1)(op:(A1,A1)=>F[A1]):F[A1]=
    shiftedStateFold(c,monad)(z, op, identity)
    
  def foldLeft[F[_],B](c:CA,monad: CpsMonad[F])(z:B)(op:(B,A)=>F[B]):F[B]=
    c.foldLeft(monad.pure(z)){ (s,e) =>
       monad.flatMap(s)(si => op(si,e))
    }

  def foldRight[F[_],B](c:CA,monad: CpsMonad[F])(z:B)(op:(A,B)=>F[B]):F[B]=
    c.foldRight(monad.pure(z)){ (e,s) =>
       monad.flatMap(s)(si => op(e, si))
    }

  def groupMapReduce[F[_],K,B](c:CA, monad: CpsMonad[F])(key: (A)=>F[K])(f: A=>F[B])(reduce: (B,B) => F[B]): 
                                                                                      F[immutable.Map[K,B]] = 
    shiftedStateFold(c,monad)(
      prolog = mutable.Map[K,B](),
      acc = (s, a) => {
         val fk = key(a); 
         val fb = f(a)
         monad.flatMap(fk)(k => monad.flatMap(fb){b =>  
            s.get(k) match
              case Some(sb) => 
                  monad.map(reduce(sb,b)){x => 
                    s.updated(k,x)
                  }
              case None => s.update(k,b)
                  monad.pure(s)
         })
      },
      epilog = _.toMap
    )

  def maxByOpOption[F[_],B](c:CA, monad: CpsMonad[F])(f: A=>F[B])(cmp: (B, B) => Int ):  F[Option[A]] =
     val s0: Option[(A,B)] = None
     val r = c.foldLeft(monad.pure(s0)){(s,a) =>
         monad.flatMap(s){si => 
           si match
             case None => monad.map(f(a))(x => Some((a,x)))
             case old@Some((as,bs)) => monad.map(f(a)){ b =>
                if (cmp(bs,b) < 0)
                   Some((a,b)) 
                else
                   old
             }
         }
     }
     monad.map(r)(_.map(_._1))
      
         
  def maxByOption[F[_],B](c:CA, monad: CpsMonad[F])(f: A=>F[B])(implicit cmp:math.Ordering[B]):  F[Option[A]] =
       maxByOpOption[F,B](c, monad)(f)(cmp.compare(_,_))
      
       
  def maxBy[F[_],B](c:CA, monad: CpsTryMonad[F])(f: A=>F[B])(implicit cmp:math.Ordering[B]):  F[A] =
      monad.flatMap(maxByOption(c,monad)(f)(using cmp)){ x =>
        x match
           case Some(v) => monad.pure(v)
           case None => monad.error(new UnsupportedOperationException)
      }
     
  def minByOption[F[_],B](c:CA, monad: CpsMonad[F])(f: A=>F[B])(implicit cmp:math.Ordering[B]):  F[Option[A]] =
       maxByOpOption[F,B](c, monad)(f)(- cmp.compare(_,_))
      
  def minBy[F[_],B](c:CA, monad: CpsTryMonad[F])(f: A=>F[B])(implicit cmp:math.Ordering[B]):  F[A] =
      monad.flatMap(minByOption(c,monad)(f)(using cmp)){ x =>
        x match
           case Some(v) => monad.pure(v)
           case None => monad.error(new UnsupportedOperationException)
      }



}

class IterableOpsAsyncShift[A, C[X] <: Iterable[X] & IterableOps[X,C,C[X]], CA <: C[A] ] 
                                                        extends IterableAsyncShift[A,CA] {



  def map[F[_], B](c: CA, monad: CpsMonad[F])(f: A=> F[B]):F[C[B]] = 
    shiftedFold(c,monad)(
      c.iterableFactory.newBuilder[B], 
      f,
      (s,a,b) => s.addOne(b),
      _.result
    )
    
  def flatMap[F[_], B](c: CA, monad: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[C[B]] = 
    shiftedFold(c,monad)(
      c.iterableFactory.newBuilder[B],
      f,
      (s,a,b) => s.addAll(b),
      _.result
    )

  def collect[F[_], B](c: CA, monad: CpsMonad[F])(pf: PartialFunction[A,F[B]]):F[C[B]] =
    shiftedFold(c,monad)(
      c.iterableFactory.newBuilder[B],
      x => (pf.lift)(x) match {
              case Some(v) => monad.map(v)(Some(_))
              case None => monad.pure(None)
      },
      (s,a,b) => b match {
                   case Some(v) => s.addOne(v)
                   case None => s
                 },
      _.result
    )


  def dropWhile[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    shiftedWhile(c,monad)(c:C[A],p,(s,c,a)=>if (c) s.drop(1) else s,identity)
    

  def filter[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    shiftedFold(c,monad)(
       c.iterableFactory.newBuilder[A],p,
       (s,a,b) => if (b) s.addOne(a) else s,
       _.result
    )

  def filterNot[F[_]](c:CA, monad: CpsMonad[F])(p: A=>F[Boolean]):F[C[A]] =
    filter(c,monad)(a => monad.map(p(a))(! _))


  def flatten[F[_],B](c:CA,monad: CpsMonad[F])(implicit asIterable: (A)=> F[IterableOnce[B]]):F[C[B]]=
    shiftedFold(c,monad)(
       c.iterableFactory.newBuilder[B],
       asIterable,
       (s,a,b) => s.addAll(b),
       _.result    
    )
                          
  def groupBy[F[_],K](c:CA,monad: CpsMonad[F])(f: (A)=>F[K] ):F[immutable.Map[K,C[A]]] =
    shiftedFold(c,monad)(
      prolog = mutable.Map[K,mutable.Builder[A,C[A]]](),
      action = f,
      acc = (s,a,b)=> {
        s.get(b) match
          case Some(itb) => itb.addOne(a); 
          case None => 
                val itb = c.iterableFactory.newBuilder[A]
                itb.addOne(a)
                s.addOne(b,itb)
        s
      }, 
      epilog = _.view.mapValues(_.result).toMap
    )

 
  def groupMap[F[_],K,B](c:CA,monad: CpsMonad[F])(key: (A)=>F[K])(f: A=>F[B]):F[immutable.Map[K,C[B]]] =
    shiftedStateFold(c,monad)(
      prolog = mutable.Map[K,mutable.Builder[B,C[B]]](),
      acc = (s, a) => {
         val fk = key(a); 
         val fb = f(a)
         monad.flatMap(fk)(k => monad.map(fb){b =>  
            s.get(k) match
              case Some(itb) => itb.addOne(b)
                                s               
              case None =>
                val itb = c.iterableFactory.newBuilder[B]
                itb.addOne(b)
                s.addOne(k,itb)
         })
      },
      epilog = _.view.mapValues(_.result).toMap
    )

  def scanLeft[F[_],B](c:CA,monad: CpsMonad[F])(z: B)(op: (B,A) => F[B]):F[C[B]] =
    shiftedStateFold(c,monad)(
      prolog = {
         val s = c.iterableFactory.newBuilder[B]
         s.addOne(z)
         (s,z)
      },
      acc = (s,a) => {
         val (it, pb) = s
         monad.map(op(pb,a)){ b=>
           it.addOne(b)
           (it,b)
         }
      },
      epilog = _._1.result
    )


}


