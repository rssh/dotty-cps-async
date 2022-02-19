package cps.stream

import cps.*
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.*
import scala.util.*


/**
 * Minimal mutable async stream.
 **/
trait AsyncIterator[F[_]:CpsConcurrentMonad, T]:

  thisAsyncIterator =>

  /**
  * return the next element of stream in option or None if stream is finished.
  **/
  def  next: F[Option[T]]


  def  map[S](f: T=>S): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next: F[Option[S]] = 
      summon[CpsConcurrentMonad[F]].map(thisAsyncIterator.next)(_.map(f))
  }

  def  mapAsync[S](f: T=> F[S]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next: F[Option[S]] = 
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => summon[CpsConcurrentMonad[F]].map(f(v))(Some(_))
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }

  def filter(p: T=>Boolean): AsyncIterator[F,T] = new AsyncIterator[F, T] {
    def next: F[Option[T]] =
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => if (p(v)) summon[CpsConcurrentMonad[F]].pure(Some(v)) else next
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }

  def filterAsync(p: T=>F[Boolean]): AsyncIterator[F,T] = new AsyncIterator[F, T] {
    def next: F[Option[T]] =
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => summon[CpsConcurrentMonad[F]].flatMap(p(v)){ c =>
                if (c) then 
                  summon[CpsConcurrentMonad[F]].pure(Some(v))
                else
                  next
            }
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }

  def fold[S](s0:S)(f:(S,T)=>S): F[S] = 
    summon[CpsConcurrentMonad[F]].flatMap(next){ 
      case Some(v) => fold(f(s0,v))(f)
      case None => summon[CpsConcurrentMonad[F]].pure(s0)
    }
  

  def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S] =
    summon[CpsConcurrentMonad[F]].flatMap(next){
      case Some(v) =>
        summon[CpsConcurrentMonad[F]].flatMap(f(s0,v)){ s => foldAsync(s)(f) }
      case None => summon[CpsConcurrentMonad[F]].pure(s0)  
    }


object AsyncIterator:

   def unfold[S,F[_]:CpsConcurrentMonad,T](s0:S)(f:S => F[Option[(T,S)]]): AsyncIterator[F,T] =
     AsyncListIterator(AsyncList.unfold(s0)(f))

   given absorber[F[_],C<:CpsMonadContext[F],T](using ExecutionContext, CpsConcurrentMonad.Aux[F,C]): CpsAsyncEmitAbsorber4[AsyncIterator[F,T],F,C,T] =
     AsyncIteratorEmitAbsorber[F,C,T]()

     

class AsyncIteratorEmitAbsorber[F[_],C<:CpsMonadContext[F],T](using ec: ExecutionContext, auxAsyncMonad: CpsConcurrentMonad.Aux[F,C]) extends CpsAsyncEmitAbsorber4[AsyncIterator[F,T],F,C,T]:

  override type Element = T
  override type Context = C

  override val asyncMonad = auxAsyncMonad

  override def eval(f: C => CpsAsyncEmitter[Monad,Element] => Monad[Unit]): AsyncIterator[F,T] =
     val list = AsyncListEmitAbsorber[F,C,T].eval(f)
     AsyncListIterator(list)



