package cps.stream

import cps.*

import scala.concurrent.*
import scala.util.*

class AsyncListIterator[F[_]:CpsConcurrentMonad, T](l: AsyncList[F,T])(using ExecutionContext) extends AsyncIterator[F,T]:

    sealed trait State
    case class StateInit(l:AsyncList[F,T]) extends State
    case class StateNext(cachedNext: Promise[Option[(T,AsyncList[F,T])]]) extends State

    var  state: State = StateInit(l)

    def next:F[Option[T]] = {
      val m = summon[CpsAsyncMonad[F]]
      val p=Promise[Option[(T,AsyncList[F,T])]]
      val nState = StateNext(p)
      val nNext: F[Option[(T,AsyncList[F,T])]] = {
        state match
          case StateInit(l) => l.next
          case StateNext(cachedPrev) =>
            val prevNext = m.adoptCallbackStyle{ (cb:Try[Option[(T,AsyncList[F,T])]]=>Unit) =>
              cachedPrev.future.onComplete(cb)
            }
            m.flatMap(prevNext){ (v: Option[(T,AsyncList[F,T])]) =>
              v match
                case Some((e,l)) => l.next
                case None => m.pure(None)
            }
      }
      state = nState
      m.flatMapTry(nNext){r => 
        p.complete(r)
        r match
          case Failure(ex) => summon[CpsMonad[F]].error(ex)
          case Success(v) =>
            val oe = v.map(_._1)
            summon[CpsMonad[F]].pure(oe)
      }
    }

    /*
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
    */
