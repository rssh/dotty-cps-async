package cps.stream

import scala.concurrent.*
import scala.util.*
import java.util.concurrent.atomic.AtomicReference
import cps.*



class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T])(using ExecutionContext) extends AsyncIterator[F,T]:
   

   case class State(
      current: AsyncList[F,T],
      nAdvances: Int,
      cachedNext: Promise[Option[(T,AsyncList[F,T])]]
   )
   val refState:  AtomicReference[State] = new AtomicReference(State(l,0,Promise()))

   
   def next: F[Option[T]] = {
      val m = summon[CpsAsyncMonad[F]]
      var retval: F[Option[T]]|Null = null
      while(retval == null) {  
         var state = refState.get.nn
         val p=Promise[Option[(T,AsyncList[F,T])]]
         val nState = State(state.current,state.nAdvances+1,p)
         if (refState.compareAndSet(state,nState)) {
             val nNext: F[Option[(T,AsyncList[F,T])]] = {
               if state.nAdvances == 0 then
                  state.current.next
               else 
                  m.adoptCallbackStyle{ cb =>
                     state.cachedNext.future.onComplete(cb)
                  }
             }
             retval = m.flatMapTry(nNext)(n => finishNext(p,n))
         }
      }
      retval
   }

   private def finishNext(p:Promise[Option[(T,AsyncList[F,T])]],r:Try[Option[(T,AsyncList[F,T])]]):F[Option[T]] = {
      var retval: F[Option[T]]| Null = null
      r match
         case Success(v) =>
            val state = refState.get.nn
            v match
               case Some((e,l)) =>
                  while(retval == null) {
                     val nState = {
                        if state.nAdvances == 1 then 
                           State(l,0,p) 
                        else 
                           state.copy(nAdvances = state.nAdvances - 1)
                     }
                     if (refState.compareAndSet(state,nState)) then
                        retval = summon[CpsMonad[F]].pure(Some(e))
                  }
               case None =>
                  val nState = state.copy(current=AsyncList.empty)
                  while(retval == null) {
                     if (refState.compareAndSet(state,nState)) {
                        retval = summon[CpsMonad[F]].pure(None)
                     }
                  }
         case Failure(ex) =>
            retval = summon[CpsTryMonad[F]].error(ex)
      p.complete(r)
      retval.nn
   }



   /*
   def  next1: F[Option[T]] =
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
   */
     
        
end AsyncListIterator