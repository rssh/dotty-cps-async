package cps

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration._
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicInteger

  
 

class ASChannel[F[_]:CpsAsyncMonad,A]
{
  

   private val readers: ConcurrentLinkedQueue[A => F[Unit]] = new ConcurrentLinkedQueue()
   private val writers: ConcurrentLinkedQueue[Unit=>F[A]] = new ConcurrentLinkedQueue()

   def read():F[A] = {
      Option(writers.poll()) match 
        case Some(writer) =>
                writer(())
        case None =>
                val m = summon[CpsAsyncMonad[F]]
                m.adoptCallbackStyle[A]( callback =>
                   readers.add(x => m.pure(callback(Success(x))))
                )
   }

   def write(a:A):F[Unit] = {
      Option(readers.poll()) match 
        case Some(reader) =>
                  reader(a)
        case None =>
                val m = summon[CpsAsyncMonad[F]]
                m.adoptCallbackStyle[Unit]( callback =>
                    writers.add(  x => m.pure{ callback(Success(())); a} )
                )
   }
 

}

