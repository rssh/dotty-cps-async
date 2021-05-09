package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.Builder

class IterableAsyncShift[A, CA <: Iterable[A] ] extends AsyncShift[CA] {

  /**
   * sequentially do action. each action is started after prev. is finished
   */
  def shiftedFold[F[_],S,B,R](c:CA, monad:CpsMonad[F])(
                                      prolog:S, 
                                      action: A=>F[B],
                                      acc: (S,A,B)=> S, 
                                      epilog:S=>R):F[R] =
   def advance(it: Iterator[A], s:S):F[S] =
       if it.hasNext then
          val a = it.next()
          monad.flatMap(action(a)){ b =>
             advance(it,acc(s,a,b))
          }
       else
          monad.pure(s)
   monad.map(advance(c.iterator, prolog))(epilog)

   
  def shiftedStateFold[F[_],S,R](c:CA, monad: CpsMonad[F])(
                            prolog: S,
                            acc: (S,A) => F[S],
                            epilog:S=>R):F[R] =
   def advance(it: Iterator[A], s:S):F[S] =
       if it.hasNext then
          val a = it.next()
          monad.flatMap(acc(s,a))(s => advance(it, s))
       else
          monad.pure(s)
   monad.map(advance(c.iterator, prolog))(epilog)

    
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
                    s.addOne((k,x))
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

  def reduceOption[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (B,B) => F[B]): F[Option[B]] = 
      val s0: F[Option[B]] = monad.pure(None)
      c.foldLeft(s0)((fs,e)=>
        monad.flatMap(fs){ s =>
          s match
            case Some(v) => monad.map(op(v,e))(x => Some(x))
            case None => monad.pure(Some(e))
        } 
      )

  def reduce[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (B,B) => F[B]): F[B] = 
      try
        c.view.map(monad.pure(_)).reduce( (fx,fy) => 
          monad.flatMap(fx)(x=>monad.flatMap(fy)(y => op(x,y))) )
      catch
        case ex: UnsupportedOperationException =>
          monad.error(ex)
      
  def reduceLeft[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (B,A) => F[B]): F[B] = 
       monad.map(reduceLeftOption(c,monad)(op))(_.get)
      
  def reduceLeftOption[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (B,A) => F[B]): F[Option[B]] = 
       val s0:F[Option[B]] = monad.pure(None)
       c.foldLeft(s0){ (s,e) =>
         monad.flatMap(s){ c =>
            c match
              case Some(b) => monad.map(op(b,e))(x => Some(x))
              case None => monad.pure(Some(e))
         }
       }

  def reduceRightOption[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (A,B) => F[B]): F[Option[B]] = 
       val s0:F[Option[B]] = monad.pure(None)
       c.foldRight(s0){ (e,s) =>
         monad.flatMap(s){ c =>
            c match
              case Some(b) => monad.map(op(e,b))(x => Some(x))
              case None => monad.pure(Some(e))
         }
       }

  def reduceRight[F[_],B>:A](c:CA, monad: CpsTryMonad[F])(op: (A,B) => F[B]): F[B] = 
       monad.map(reduceRightOption(c,monad)(op))(_.get)
      

}

class IterableOpsAsyncShift[A, C[X] <: Iterable[X] & IterableOps[X,C,C[X]], CA <: C[A] ] 
                                                        extends IterableAsyncShift[A,CA] {


  def _cpsWithFilterSubst(ca:CA, predicate: A=>Boolean) =
        WithFilterSubstAsyncShift[A,C,CA](ca, predicate, this)

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
    shiftedWhile(c,monad)(0,p,(s,c,a)=>if (c) s+1 else s, n => if (n>0) c.drop(n) else c)
    

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

  def scanRight[F[_],B](c:CA,monad: CpsMonad[F])(z: B)(op: (A,B) => F[B]):F[C[B]] =
    val it0 = c.iterableFactory.newBuilder[B]
    it0.addOne(z)
    val s0 = monad.pure((it0, z))
    val itb = c.foldRight(s0){ (e,fs) =>
       monad.flatMap(fs){ s =>
          val fnb = op(e,s._2)
          monad.map(fnb){ nb =>
             s._1.addOne(nb)
             (s._1,nb)
          }
       }
    }
    monad.map(itb)(_._1.result)


  // TODO: make abstract here, different impl
  def span[F[_]](c:CA,monad: CpsMonad[F])(p: (A) => F[Boolean]):F[(C[A],C[A])] = 
    shiftedWhile(c, monad)( 
       (c.iterableFactory.newBuilder[A], 0, true),
       p,
       (s,c,a) => {
         if (c) 
           s._1.addOne(a)
           (s._1, s._2+1, true)
         else
            s
       },
       s => (s._1.result(), if (s._2 > 0)  c.drop(s._2) else c)
    )

  def takeWhile[F[_]](c:CA,monad: CpsMonad[F])(p: (A) => F[Boolean]):F[C[A]] = 
    shiftedWhile(c, monad)( 
       c.iterableFactory.newBuilder[A],
       p,
       (s,c,a) => {
         if (c) s.addOne(a)
         s
       },
       _.result
    )

  def partition[F[_]](c:CA,monad: CpsMonad[F])(p: (A) => F[Boolean]):F[(C[A],C[A])] = 
    shiftedFold(c,monad)(
       ( c.iterableFactory.newBuilder[A], c.iterableFactory.newBuilder[A] ), 
       p,
       (s,a,pb) => {
         (if (pb) s._1 else s._2).addOne(a)
         s
       },
       (s => (s._1.result, s._2.result) )
    )

  def partitionMap[F[_], A1, A2](c:CA,monad: CpsMonad[F])(f: (A) => F[Either[A1,A2]]):F[(C[A1],C[A2])] = 
    shiftedFold(c,monad)(
       ( c.iterableFactory.newBuilder[A1], c.iterableFactory.newBuilder[A2] ), 
       f,
       (s,a,e) => {
         e match
            case Left(a1) => s._1.addOne(a1)
            case Right(a2) => s._2.addOne(a2)
         s
       },
       s => (s._1.result, s._2.result)
    )
  
  def tapEach[F[_],U](c: CA, monad: CpsMonad[F])(f: A => F[U]): F[C[A]] = 
      monad.map(foreach(c, monad)(f))(_ => c)


  //  currently
  def withFilter[F[_]](c: CA, m: CpsMonad[F])(p: A => F[Boolean]): DelayedWithFilter[F,A,C,CA] =
      DelayedWithFilter(c,m,p)


}

  


class DelayedWithFilter[F[_], A, C[X] <: Iterable[X]  & IterableOps[X,C,C[X]], CA <: C[A]](c: CA, 
                                         m: CpsMonad[F], 
                                         p:A=>F[Boolean], 
                                         ) extends CallChainAsyncShiftSubst[F, WithFilter[A,C], F[WithFilter[A,C]] ]
{

  // return eager copy
  def _origin: F[WithFilter[A,C]] = {
     m.map(runScan[A]((s,a)=>s.addOne(a)))(_.withFilter(_ => true))
  }

  def runScan[B](iterateEffect: (Builder[B,C[B]],A) => Unit ): F[C[B]] =
     val s0 = m.pure(c.iterableFactory.newBuilder[B])
     val s = c.foldLeft(s0){(fs,e) =>
        m.flatMap(fs){ s =>
          m.map(p(e)){ r =>
             if (r) iterateEffect(s,e)
             s
          }
        }
     }
     m.map(s)(_.result)
     
  def runScanF[B](iterateChange: (Builder[B,C[B]],A) => F[Builder[B,C[B]]] ): F[C[B]] =
     val s0 = m.pure(c.iterableFactory.newBuilder[B])
     val s = c.foldLeft(s0){(fs,e) =>
        m.flatMap(fs){ s =>
          m.flatMap(p(e)){ r =>
             if (r) 
               iterateChange(s,e)
             else
               m.pure(s)
          }
        }
     }
     m.map(s)(_.result)
     

  // this path (without proxy using) will be substitued
  def map[B](f: A => B): F[C[B]] = 
     runScan((s,a)=>s.addOne(f(a)))

  def map_async[B](f: A => F[B]): F[C[B]] =
     runScanF((s,a)=>{ 
       m.map(f(a))(b => {
         s.addOne(b)
         s
       })
     })


  def flatMap[B](f: A => IterableOnce[B]): F[C[B]] = 
     runScan((s,a)=>s.addAll(f(a)))

  def flatMap_async[B](f: A => F[IterableOnce[B]]): F[C[B]] = 
     runScanF((s,a)=>{ 
       m.map(f(a))(b => {
         s.addAll(b)
         s
       })
     })

  def withFilter(q: A=>Boolean) = DelayedWithFilter(c, m, x => m.map(p(x))(r => r && q(x)) )

  def withFilter_async(q: A=> F[Boolean]) = 
       DelayedWithFilter(c, m, x => m.flatMap(p(x)){r => 
                                if (r) then
                                     q(x) 
                                else
                                     m.pure(false)
                         })

  def foreach[U](f: A=>U): F[Unit] = {
     val s0 = m.pure(())
     c.foldLeft(s0){(fs,e) =>
        m.flatMap(fs){ s =>
          m.map(p(e)){ r =>
            if (r) f(e)
            ()
          }
         }
     }
  }

  def foreach_async[U](f: A=>F[U]): F[Unit] = {
     val s0 = m.pure(())
     c.foldLeft(s0){(fs,e) =>
        m.flatMap(fs){ s =>
          m.flatMap(p(e)){ r =>
             if (r) m.map(f(e))(_ => ()) else m.pure(())
          }
        }
     }
  }

}



