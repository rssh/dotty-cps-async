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

   given absorber[F[_]:CpsConcurrentMonad,T](using ExecutionContext): CpsAsyncEmitAbsorber3[AsyncIterator[F,T],F,T] =
     AsyncIteratorEmitAbsorber[F,T]()

     

class AsyncIteratorEmitAbsorber[F[_]:CpsConcurrentMonad,T](using ExecutionContext) extends BaseUnfoldCpsAsyncEmitAbsorber[AsyncIterator[F,T],F,T]:

  override type Element = T

  def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): AsyncIterator[F,T] =
        AsyncIterator.unfold[S,F,T](s0)(f)


