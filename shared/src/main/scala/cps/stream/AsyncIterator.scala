package cps.stream

import cps.*
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.*
import scala.util.*


/**
 * Minimal mutable async stream.
 **/
trait AsyncIterator[F[_]:CpsConcurrentMonad, T]:

   def  next: F[Option[T]]



object AsyncIterator:

   def unfold[S,F[_]:CpsConcurrentMonad,T](s0:S)(f:S => F[Option[(T,S)]]): AsyncIterator[F,T] =
     AsyncListIterator(AsyncList.unfold(s0)(f))

   given absorber[F[_]:CpsConcurrentMonad,T](using ExecutionContext): CpsAsyncEmitAbsorber3[AsyncIterator[F,T],F,T] =
     AsyncIteratorEmitAbsorber[F,T]()




class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T]) extends AsyncIterator[F,T]:
   val  ref = new AtomicReference[AsyncList[F,T]](l)

   def  next: F[Option[T]] =
     val m = summon[CpsAsyncMonad[F]]
     m.adoptCallbackStyle[Option[T]]{ elementCallback =>
         ref.updateAndGet{ lStart =>
            val delayedList = m.adoptCallbackStyle[AsyncList[F,T]]{
              listCallback => m.mapTry(lStart.next){ 
                 case Success(v) =>
                   v match
                      case Some((e,l)) => elementCallback(Success(Some(e)))
                                     listCallback(Success(l))
                      case None => elementCallback(Success(None))
                                     listCallback(Success(AsyncList.empty))
                 case Failure(ex) =>
                         elementCallback(Failure(ex))
                         // or also failure ?  need rethinl.
                         listCallback(Success(AsyncList.empty))
              }
            }
            AsyncList.Wait(delayedList)
         }
     }
        

class AsyncIteratorEmitAbsorber[F[_]:CpsConcurrentMonad,T](using ExecutionContext) extends BaseUnfoldCpsAsyncEmitAbsorber[AsyncIterator[F,T],F,T]:

  override type Element = T

  def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): AsyncIterator[F,T] =
        AsyncIterator.unfold[S,F,T](s0)(f)


