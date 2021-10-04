package cps.stream

import cps.*

import scala.util.*

class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T]) extends AsyncIterator[F,T]:
   var  ref = l

   def  next: F[Option[T]] =
     val m = summon[CpsAsyncMonad[F]]
     m.adoptCallbackStyle[Option[T]]{ elementCallback =>
          val lStart = ref
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
          ref = AsyncList.Wait(delayedList)
     }
