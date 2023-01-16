package cps.stream


import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.*
import scala.util.*

import cps.*


class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T])(using ExecutionContext) extends AsyncIterator[F,T]:

  sealed trait State
  case class StateInit(l:AsyncList[F,T]) extends State
  case class StateNext(cachedNext: Promise[Option[(T,AsyncList[F,T])]]) extends State

  // scala-native is one-threaded for now
  //  but we use thread-safe version to simplicy future changing of
  //  scala-native mode.
  val  refState: AtomicReference[State] = new AtomicReference(StateInit(l))


  def  next: F[Option[T]] = {
     val m = summon[CpsAsyncMonad[F]]
     var retval: F[Option[T]]|Null = null
     while(retval == null) {  
        val state = refState.get.nn
        val p=Promise[Option[(T,AsyncList[F,T])]]()
        val nState = StateNext(p)
        if (refState.compareAndSet(state,nState)) {
            val nNext: F[Option[(T,AsyncList[F,T])]] = {
              state match
                 case StateInit(l) =>
                    l.next
                 case StateNext(cachedNext) =>
                    val prevNext = m.adoptCallbackStyle{ (cb:Try[Option[(T,AsyncList[F,T])]]=>Unit) =>
                       cachedNext.future.onComplete(cb)
                    }
                    m.flatMap(prevNext){ (v: Option[(T,AsyncList[F,T])]) =>
                       v match
                          case Some((e,l)) => l.next
                          case None        => m.pure(None)
                    }
            }
            retval = m.flatMapTry(nNext)(n => finishNext(p,n))
        }
     }
     retval.nn
  }

  private def finishNext(p:Promise[Option[(T,AsyncList[F,T])]],r:Try[Option[(T,AsyncList[F,T])]]):F[Option[T]] = {
    p.complete(r)
    r match
       case Success(v) =>
          v match
             case Some((e,l)) =>
                summon[CpsMonad[F]].pure(Some(e))
             case None =>
                summon[CpsMonad[F]].pure(None)
       case Failure(ex) =>
          summon[CpsTryMonad[F]].error(ex)
 }


end AsyncListIterator