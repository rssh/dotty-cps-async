package cps.stream

import cps.*
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.*
import scala.collection.mutable.AbstractBuffer
import scala.collection.mutable.ListBuffer
import scala.util.*

/**
 * Mininal async stream.
 **/
sealed trait AsyncList[F[_]:CpsConcurrentMonad, +T]:

  def next: F[Option[(T,AsyncList[F,T])]@uncheckedVariance] 

  def map[S](f: T=>S): AsyncList[F,S]

  def mapAsync[S](f: T=> F[S]): AsyncList[F,S]

  def flatMap[S](f: T=>AsyncList[F,S]): AsyncList[F,S]

  def append[S >: T](x: =>AsyncList[F,S]): AsyncList[F,S]

  def appendAsync[S >: T](x: ()=>F[AsyncList[F,S]]): AsyncList[F,S] =
          append(AsyncList.Wait(x()))

  def fold[S](s0:S)(f:(S,T)=>S): F[S] 

  def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S]

  def scan[S](s0:S)(f:(S,T)=>S): AsyncList[F,S] =
          AsyncList.Cons(s0, ()=>scanTail(s0)(f))

  def scanTail[S](s0:S)(f:(S,T)=>S): AsyncList[F,S]

  def scanAsync[S](s0:S)(f:(S,T)=>F[S]): AsyncList[F,S] =
          AsyncList.Cons(s0, () => scanTailAsync(s0)(f))

  def scanTailAsync[S](s0:S)(f:(S,T)=>F[S]): AsyncList[F,S]
  
  def foreach[U](f: T=>U): F[Unit] =
       fold(())((s,t)=>{ f(t); () })

  def foreachAsync[U](f: T=>F[U]): F[Unit] =
       foldAsync(())((s,t)=> summon[CpsMonad[F]].map(f(t))(_ => ()))

  def filter(p: T=>Boolean):  AsyncList[F,T]

  def filterAsync(p: T=>F[Boolean]): AsyncList[F,T]

  def takeList(n:Int): F[List[T]@uncheckedVariance] =
       summon[CpsMonad[F]].map(takeTo( new ListBuffer(), n))(_.toList)

  def takeListAll(): F[List[T]@uncheckedVariance] =
       takeList(-1)

  def takeTo[B <: AbstractBuffer[T]@uncheckedVariance](buffer: B, n: Int):F[B]       
  
  def merge[S >: T](other: AsyncList[F,S]): AsyncList[F,S]

  def iterator: AsyncIterator[F,T@uncheckedVariance] =
        new AsyncListIterator(this)


object AsyncList {

  case class Wait[F[_]:CpsConcurrentMonad, T](fs: F[AsyncList[F,T]])  extends AsyncList[F,T]:

     def next: F[Option[(T,AsyncList[F,T])]] =
          summon[CpsMonad[F]].flatMap(fs)(_.next)

     def map[S](f: T=>S): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)( _.map(f) ))

     def mapAsync[S](f: T=>F[S]): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)( _.mapAsync(f) ))
    
     def flatMap[S](f: T=>AsyncList[F,S]): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)( _.flatMap(f) ))

     def append[S >: T](x: =>AsyncList[F,S]): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)( _.append(x) ))

     def fold[S](s0:S)(f:(S,T)=>S): F[S] =
          summon[CpsMonad[F]].flatMap(fs)( _.fold(s0)(f) )
         
     def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S] =
          summon[CpsMonad[F]].flatMap(fs)( _.foldAsync(s0)(f) )

     def scanTail[S](s0:S)(f:(S,T)=>S): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)(_.scanTail(s0)(f)))

     def scanTailAsync[S](s0:S)(f:(S,T)=>F[S]): AsyncList[F,S] =
          Wait(summon[CpsMonad[F]].map(fs)(_.scanTailAsync(s0)(f)))

     def filter(p: T=>Boolean):  AsyncList[F,T] =
          Wait(summon[CpsMonad[F]].map(fs)(_.filter(p)))
                    
     def filterAsync(p: T=>F[Boolean]): AsyncList[F,T] =
          Wait(summon[CpsMonad[F]].map(fs)(_.filterAsync(p)))

     def takeTo[B <: AbstractBuffer[T]](buffer: B, n: Int):F[B] =
          if n == 0 then
               summon[CpsMonad[F]].pure(buffer)
          else
               summon[CpsMonad[F]].flatMap(fs)(_.takeTo(buffer,n))      
         
     def merge[S >: T](other: AsyncList[F,S]): AsyncList[F,S] =
          other match
               case Wait(fs1) =>
                    val fs1cast = fs1.asInstanceOf[F[AsyncList[F,S]]]
                    val next = summon[CpsMonad[F]].map(summon[CpsConcurrentMonad[F]].concurrently(fs,fs1cast)){ r =>
                         r match
                              case Left((trs,fs1spawn)) =>
                                   trs match
                                        case Success(v) => 
                                             v merge Wait(summon[CpsConcurrentMonad[F]].join(fs1spawn))
                                        case Failure(ex) =>
                                             Wait(summon[CpsAsyncMonad[F]].error(ex))
                              case Right((fs0spawn, fs1r)) =>
                                   fs1r match
                                        case Success(v) =>
                                             v merge Wait(summon[CpsConcurrentMonad[F]].join(fs0spawn))
                                        case Failure(ex) =>
                                             Wait(summon[CpsAsyncMonad[F]].error(ex))
                    }
                    Wait(next)
               case Cons(s, t) =>
                    Cons(s, () => this.merge(t()))
               case Empty() => this
                    





  case class Cons[F[_]:CpsConcurrentMonad,T](head:T, tailFun: ()=>AsyncList[F,T]) extends AsyncList[F,T]:

     def next: F[Option[(T,AsyncList[F,T])]] =
          summon[CpsMonad[F]].pure(Some(head,tailFun()))

     def map[S](f: T=>S): AsyncList[F,S] =
          Cons(f(head),()=>tailFun().map(f))

     def mapAsync[S](f: T=>F[S]): AsyncList[F,S] =
          Wait(
             summon[CpsMonad[F]].map(f(head))( h =>
                Cons(h, ()=>tailFun().mapAsync(f))
             )
          ) 

     def flatMap[S](f: T=>AsyncList[F,S]): AsyncList[F,S] =
          f(head).append( tailFun().flatMap(f) )

     def append[S >: T](x: =>AsyncList[F,S]): AsyncList[F,S] =
          Cons(head, ()=>tailFun().append(x))

     def fold[S](s0:S)(f:(S,T)=>S): F[S] =
          tailFun().fold(f(s0,head))(f)

     def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S] =
          summon[CpsMonad[F]].flatMap(f(s0,head))( s => tailFun().foldAsync(s)(f) )

     def scanTail[S](s0:S)(f:(S,T)=>S): AsyncList[F,S] =
          val s1 = f(s0,head)
          Cons(s1, () => tailFun().scanTail(s1)(f))
     
     def scanTailAsync[S](s0:S)(f:(S,T)=>F[S]): AsyncList[F,S] =
          Wait(
               summon[CpsAsyncMonad[F]].map((f(s0,head))){ s1 =>
                   Cons(s1, ()=>tailFun().scanTailAsync(s1)(f))
               }  
          )
     

     def filter(p: T=>Boolean):  AsyncList[F,T] =
          if p(head) then
               Cons(head, () => tailFun().filter(p))
          else
               tailFun().filter(p)
          
     def filterAsync(p: T=>F[Boolean]): AsyncList[F,T] =
          Wait(summon[CpsAsyncMonad[F]].map(p(head)){ c =>
               if c then
                    Cons(head,() => tailFun().filterAsync(p))
               else
                    tailFun().filterAsync(p)
          })

     def takeTo[B <: AbstractBuffer[T]](buffer: B, n: Int):F[B] =
          if (n == 0) then
               summon[CpsMonad[F]].pure(buffer)
          else
            var next: AsyncList[F,T] = this
            var current: Cons[F, T] = this
            var endLoop = false
            var nRest = n
            while(nRest != 0 && !endLoop) {
               buffer.addOne(current.head)
               next = current.tailFun()   
               next match
                    case c: Cons[_,_] =>
                         current = c.asInstanceOf[Cons[F,T]]
                    case _ =>
                         endLoop = true
               nRest = nRest - 1
            }
            next.takeTo(buffer, nRest)
     
     def merge[S >: T](other: AsyncList[F,S]): AsyncList[F,S] =
          Cons(head, ()=>other.merge(tailFun()))

      
            

  case class Empty[F[_]: CpsConcurrentMonad]() extends AsyncList[F,Nothing]:

     def next: F[Option[(Nothing,AsyncList[F,Nothing])]] =
          summon[CpsMonad[F]].pure(None)

     def map[S](f: Nothing=>S): AsyncList[F,S] = this

     def mapAsync[S](f: Nothing=>F[S]): AsyncList[F,S] = this

     def flatMap[S](f: Nothing=> AsyncList[F,S]): AsyncList[F,S] = this

     def append[S >: Nothing](x: =>AsyncList[F,S]): AsyncList[F,S] = x

     def fold[S](s0:S)(f:(S,Nothing)=>S): F[S] = summon[CpsMonad[F]].pure(s0)

     def foldAsync[S](s0:S)(f:(S,Nothing)=> F[S]): F[S] = summon[CpsMonad[F]].pure(s0)

     def scanTail[S](s0:S)(f:(S,Nothing)=>S): AsyncList[F,S] = this
     
     def scanTailAsync[S](s0:S)(f:(S,Nothing)=>F[S]): AsyncList[F,S] = this
     
     def filter(p: Nothing => Boolean): Empty[F] = this
     
     def filterAsync(p: Nothing => F[Boolean]): Empty[F] = this

     def takeTo[B <: AbstractBuffer[Nothing]](buffer: B, n: Int):F[B] =
          summon[CpsMonad[F]].pure(buffer)
       
     def merge[S](other: AsyncList[F,S]): AsyncList[F,S] =
          other
     

  def empty[F[_]: CpsConcurrentMonad] : AsyncList[F,Nothing] =
     Empty[F]()


  def unfold[S,F[_]:CpsConcurrentMonad,T](s0:S)(f:S => F[Option[(T,S)]]): AsyncList[F,T] =
      Wait(
        summon[CpsConcurrentMonad[F]].map(f(s0)){
          case Some(a,s1) => Cons(a, () => unfold(s1)(f))
          case None => empty[F]
        }
      )
      


  given absorber[F[_],C<:CpsMonadContext[F],T](using ExecutionContext, CpsConcurrentMonad.Aux[F,C]): CpsAsyncEmitAbsorber4[AsyncList[F,T],F,C,T] =
          AsyncListEmitAbsorber[F,C,T]()

}


class AsyncListEmitAbsorber[F[_],C<:CpsMonadContext[F],T](using ec: ExecutionContext, auxMonad: CpsConcurrentMonad.Aux[F,C]) extends BaseUnfoldCpsAsyncEmitAbsorber[AsyncList[F,T],F,C,T]:

  override type Element = T

  def asSync(fs:F[AsyncList[F,T]]): AsyncList[F,T] =
     AsyncList.Wait(fs)

  def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): AsyncList[F,T] =
     AsyncList.unfold[S,F,T](s0)(f)



