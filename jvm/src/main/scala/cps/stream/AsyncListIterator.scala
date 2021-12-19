package cps.stream

import scala.util.*
import java.util.concurrent.atomic.AtomicReference
import cps.*



class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T]) extends AsyncIterator[F,T]:
   val  ref = new AtomicReference[AsyncList[F,T]](l)

   def  next: F[Option[T]] =
     val m = summon[CpsAsyncMonad[F]]
     //m.apply{ ctx =>
      m.adoptCallbackStyle[Option[T]]{ elementCallback =>
         ref.updateAndGet{ lStartMaybeNull =>
            val lStart = lStartMaybeNull.nn
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
     //}
        



