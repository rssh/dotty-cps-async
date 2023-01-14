package cps.stream

import cps.*

import scala.util.*

class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T]) extends AsyncIterator[F,T]:
   var  ref = l

   def  next: F[Option[T]] =
     val m = summon[CpsAsyncMonad[F]]
     println("AsyncListItertor.next")
     m.adoptCallbackStyle[Option[T]]{ elementCallback =>
          println("AsyncListItertor.next:1 => received element callback")
          val lStart = ref
          val delayedList = m.adoptCallbackStyle[AsyncList[F,T]]{
              listCallback => 
                println("AsyncListItertor.next:2 => received list callback")
                m.mapTry(lStart.next){ 
                 case Success(v) =>
                   v match
                      case Some((e,l)) => elementCallback(Success(Some(e)))
                                     listCallback(Success(l))
                      case None => elementCallback(Success(None))
                                     listCallback(Success(AsyncList.empty))
                 case Failure(ex) =>
                         elementCallback(Failure(ex))
                         // or also failure ?  need rethinl.
                         //  variants: restore lStart or permanent failure
                         listCallback(Success(AsyncList.empty))
              }
            }
          ref = AsyncList.Wait(delayedList)
     }
