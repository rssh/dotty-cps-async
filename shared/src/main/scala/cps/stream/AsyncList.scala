package cps.stream

import cps.*
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.*
import scala.collection.mutable.AbstractBuffer
import scala.collection.mutable.ListBuffer

/**
 * Mininal async stream.
 **/
sealed trait AsyncList[F[_]:CpsAsyncMonad, +T]:

  def next: F[Option[(T,AsyncList[F,T])]@uncheckedVariance] 

  def map[S](f: T=>S): AsyncList[F,S]

  def mapAsync[S](f: T=> F[S]): AsyncList[F,S]

  def flatMap[S](f: T=>AsyncList[F,S]): AsyncList[F,S]

  def append[S >: T](x: =>AsyncList[F,S]): AsyncList[F,S]

  def appendAsync[S >: T](x: ()=>F[AsyncList[F,S]]): AsyncList[F,S] =
          append(AsyncList.Wait(x()))

  def fold[S](s0:S)(f:(S,T)=>S): F[S] 

  def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S]

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
                

object AsyncList {

  case class Wait[F[_]:CpsAsyncMonad, T](fs: F[AsyncList[F,T]])  extends AsyncList[F,T]:

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


     def filter(p: T=>Boolean):  AsyncList[F,T] =
          Wait(summon[CpsMonad[F]].map(fs)(_.filter(p)))
                    
     def filterAsync(p: T=>F[Boolean]): AsyncList[F,T] =
          Wait(summon[CpsMonad[F]].map(fs)(_.filterAsync(p)))

     def takeTo[B <: AbstractBuffer[T]](buffer: B, n: Int):F[B] =
          if n == 0 then
               summon[CpsMonad[F]].pure(buffer)
          else
               summon[CpsMonad[F]].flatMap(fs)(_.takeTo(buffer,n))      
         


  case class Cons[F[_]:CpsAsyncMonad,T](head:T, tailFun: ()=>AsyncList[F,T]) extends AsyncList[F,T]:

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
          var next: AsyncList[F,T] = this
          var current: Cons[F, T] = this
          var endLoop = false
          var nRest = n
          while(nRest != 0 && !endLoop) {
               buffer.addOne(current.head)
               next = current.tailFun()   
               next match
                    case c: Cons[F,T] =>
                         current = c
                    case _ =>
                         endLoop = true
               nRest = nRest - 1
          }
          next.takeTo(buffer, nRest)
     
      
            

  case class Empty[F[_]: CpsAsyncMonad]() extends AsyncList[F,Nothing]:

     def next: F[Option[(Nothing,AsyncList[F,Nothing])]] =
          summon[CpsMonad[F]].pure(None)

     def map[S](f: Nothing=>S): AsyncList[F,S] = this

     def mapAsync[S](f: Nothing=>F[S]): AsyncList[F,S] = this

     def flatMap[S](f: Nothing=> AsyncList[F,S]): AsyncList[F,S] = this

     def append[S >: Nothing](x: =>AsyncList[F,S]): AsyncList[F,S] = x

     def fold[S](s0:S)(f:(S,Nothing)=>S): F[S] = summon[CpsMonad[F]].pure(s0)

     def foldAsync[S](s0:S)(f:(S,Nothing)=> F[S]): F[S] = summon[CpsMonad[F]].pure(s0)

     def filter(p: Nothing => Boolean): Empty[F] = this
     
     def filterAsync(p: Nothing => F[Boolean]): Empty[F] = this

     def takeTo[B <: AbstractBuffer[Nothing]](buffer: B, n: Int):F[B] =
          summon[CpsMonad[F]].pure(buffer)
       


  def empty[F[_]: CpsAsyncMonad] : AsyncList[F,Nothing] =
     Empty[F]()


  def unfold[S,F[_]:CpsAsyncMonad,T](s0:S)(f:S => F[Option[(T,S)]]): AsyncList[F,T] =
      Wait(
        summon[CpsAsyncMonad[F]].map(f(s0)){
          case Some(a,s1) => Cons(a, () => unfold(s1)(f))
          case None => empty[F]
        }
      )
      


  given absorber[F[_]:CpsConcurrentMonad,T](using ExecutionContext): CpsAsyncEmitAbsorber3[AsyncList[F,T],F,T] =
          AsyncListEmitAbsorber[F,T]()

}


class AsyncListEmitAbsorber[F[_]:CpsConcurrentMonad,T](using ec: ExecutionContext) extends BaseUnfoldCpsAsyncEmitAbsorber[AsyncList[F,T],F,T]:

  override type Element = T

  def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): AsyncList[F,T] =
        AsyncList.unfold[S,F,T](s0)(f)



