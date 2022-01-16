package cps.stream

import cps.*
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.*
import scala.util.*


/**
 * Minimal mutable async stream.
 **/
trait AsyncIterator[F[_]:CpsConcurrentMonad, T]:

  /**
  * return the next element of stream in option or None if stream is finished.
  **/
  def  next: F[Option[T]]



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



