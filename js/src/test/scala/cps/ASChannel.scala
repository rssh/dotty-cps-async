package cps

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration._
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicInteger

import cps.testconfig.given  
 

class ASChannel[F[_]:CpsAsyncMonad,A]
{
  
   final val INSERTING = 1
   final val ABANDONED = 2
   final val OK = 3

   case class ReadRecord(
      readCallback: A => F[Unit],
      readStartCallback: Boolean => Unit,
      //status: AtomicInteger
   )

   case class WriteRecord(
      writeCallback: Unit => F[A],
   )

   private val readers: ConcurrentLinkedQueue[A => F[Unit]] = new ConcurrentLinkedQueue()
   private val lastReadStarted: AtomicReference[F[Unit]|Null] = new AtomicReference(null)
   private val lastReadNumber: AtomicLong = new AtomicLong(0L)
   private val writers: ConcurrentLinkedQueue[Unit=>F[A]] = new ConcurrentLinkedQueue()
   private val lastWriteStarted: AtomicReference[F[A]|Null] = new AtomicReference(null)


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

