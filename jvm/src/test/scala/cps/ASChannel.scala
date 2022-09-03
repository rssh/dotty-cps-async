package cps

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration._
import scala.collection.mutable.ArrayDeque

  
import cps.testconfig.given

class ASChannel[F[_]:CpsAsyncMonad,A]
{
  
   //private val readers: ConcurrentLinkedQueue[A => F[Unit]] = new ConcurrentLinkedQueue()
   //private val writers: ConcurrentLinkedQueue[Unit=>F[A]] = new ConcurrentLinkedQueue()

   private val readers: ArrayDeque[A => F[Unit]] = ArrayDeque()
   private val writers: ArrayDeque[() => F[A]] = ArrayDeque()

   val m = summon[CpsAsyncMonad[F]]

   def read():F[A] = {
      val readFun: () => F[A] = this.synchronized {
         writers.removeHeadOption() match
            case Some(writer) => writer // to have writer call outsode synchronizer
            case None =>
              val fv = m.adoptCallbackStyle[A]( callback =>
                readers.append(x => m.pure(callback(Success(x))))
              ) 
              () => fv
      }
      readFun()
   }

   def write(a:A):F[Unit] = {
      val writeFun: () => F[Unit] = this.synchronized {
        readers.removeHeadOption() match
          case Some(reader) => () => reader(a)
          case None =>
            val fv = m.adoptCallbackStyle[Unit]( callback =>
              writers.append(  () => m.pure{ callback(Success(())); a} )
            )
            () => fv
      }
      writeFun()
   }
 

}

