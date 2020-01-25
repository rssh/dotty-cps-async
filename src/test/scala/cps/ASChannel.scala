package cps

import scala.collection.immutable.Queue
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.concurrent.duration._

class ASChannel[F[_]:AsyncMonad,A]
{

   private var readers: Queue[A => F[Unit]] = Queue()
   private var writers: Queue[Unit=>F[A]] = Queue()


   def read():F[A] = {
      writers.dequeueOption match 
        case Some((writer, nWriters)) =>
                writers = nWriters;
                writer(())
        case None =>
                val m = summon[AsyncMonad[F]]
                m.adoptCallbackStyle[A]( callback =>
                    readers = readers.enqueue(x => m.pure(callback(Success(x))))
                )
   }

   def write(a:A):F[Unit] = {
      readers.dequeueOption match 
        case Some((reader, nReaders)) =>
                   readers = nReaders;
                   reader(a)
        case None =>
                val m = summon[AsyncMonad[F]]
                m.adoptCallbackStyle[Unit]( callback =>
                    writers = writers.enqueue(
                                   x => m.pure{callback(Success(())); a} )
                )
   }
 

}
