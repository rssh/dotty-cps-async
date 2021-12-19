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

   val m = summon[CpsAsyncMonad[F]]

   def read():F[A] = {
      Option(writers.poll()) match 
        case Some(writer) =>
                writer.nn(())
        case None =>
                  val readFun: ()=>F[A] = this.synchronized{
                    Option(writers.poll()) match
                       case Some(writer) => () => writer.nn(())
                       case None =>
                         val fv = m.adoptCallbackStyle[A]( callback =>
                           readers.add(x => m.pure(callback(Success(x))))
                         )
                         () => fv
                  }
                  readFun.apply()
   }

   def write(a:A):F[Unit] = {
      Option(readers.poll()) match 
        case Some(reader) =>
                  reader.nn(a)
        case None =>
                val writeFun: ()=>F[Unit] = this.synchronized{
                  Option(readers.poll()) match
                    case Some(reader) => 
                       () => reader.nn(a)
                    case None =>
                       val fv = m.adoptCallbackStyle[Unit]( callback =>
                                  writers.add(  x => m.pure{ callback(Success(())); a} )
                                )
                       () => fv 
                }
                writeFun()
   }
 

}

