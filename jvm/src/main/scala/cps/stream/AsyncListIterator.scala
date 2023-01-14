package cps.stream

import scala.util.*
import java.util.concurrent.atomic.AtomicReference
import cps.*



class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T]) extends AsyncIterator[F,T]:
   val  ref = new AtomicReference[AsyncList[F,T]](l)



   /*
   def next: F[Option[T]] = {
      var retval: F[Option[T]]|Null = null
      var prevL = ref.get.nn
      var done = false
      while(!done) {
         prevL match 
            case AsyncList.Wait(fl) =>
               val nextPair = 
               val p = Promise[Option[T]]
               retval = m.adoptCallbackStyle{ listener => 
                  p.future.onComplete(listener)
               }
               val nextL = AsyncList.Wait(
                  m.flatMap(fl){ l =>   
                     m.flatMapTry(l.next){ 
                        case Success(Some((e,nl))) =>
                           p.success(Some(e))
                           nl
                        case Success(None) =>
                           p.success(None)
                           AsyncList.empty
                        case Failure(ex) =>
                           p.failure(ex)
                           // TODO:   AsyncList.failure ?
                           AsyncList.empty
                     }
                  }
               )
               done = ref.compareAndSet()
         
      }
   }
   */
   

   def  next: F[Option[T]] =
     
     val m = summon[CpsAsyncMonad[F]]


     //m.apply{ ctx =>
      m.adoptCallbackStyle[Option[T]]{ elementCallback =>
         ref.updateAndGet{ lStartMaybeNull =>
            val lStart = lStartMaybeNull.nn
            val delayedList = m.adoptCallbackStyle[AsyncList[F,T]]{ listCallback => 
                  //
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
                         listCallback(Success(AsyncList.empty))
                  }
            }
            AsyncList.Wait(delayedList)
         }
      }   
     //}
        



