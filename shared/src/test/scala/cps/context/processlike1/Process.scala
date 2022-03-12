package cps.context.processlike1

import cps.*
import scala.concurrent.*
import scala.util.*

class ProcessContext extends CpsMonadContext[Process] {

  def spawn[A](op: ProcessContext ?=> A): Process[A] =
     ??? 

  def spawnAsync[A](op: ProcessContext => Process[A]): Process[A] =
     ???

  override def adoptAwait[T](ft:Process[T]):Process[T] = ???

}

class ProcessCpsMonad() extends  CpsConcurrentMonad[Process] with CpsContextMonad[Process, ProcessContext] {

   type Spawned[A] = Process[A]

   override def pure[A](a: A): Process[A] = ???
   override def map[A,B](fa: Process[A])(f: A=>B): Process[B] = ???
   override def flatMap[A,B](fa: Process[A])(f: A=> Process[B]): Process[B] = ???
   override def error[A](e: Throwable): Process[A] = ???
   override def mapTry[A,B](fa: Process[A])(f: Try[A] => B): Process[B] = ???
   override def flatMapTry[A,B](fa: Process[A])(f: Try[A] => Process[B]): Process[B] = ???
   override def applyContext[A](op: ProcessContext => Process[A]):Process[A] = ???
   override def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): Process[A] = ???
   override def spawnEffect[A](op: =>Process[A]): Process[Process[A]] = ???
   override def join[A](op: Process[A]): Process[A] = ???
   override def tryCancel[A](op: Process[A]): Process[Unit] = ???

}



sealed trait Process[T] {

}

object Process {

  transparent inline def async[R](inline f: ProcessContext ?=> R)(using ec: ExecutionContext): Process[R] =   
    cps.async[Process].apply(f)

  def spawn[A](f: ProcessContext ?=> A)(using ProcessContext): Process[A] =
      summon[ProcessContext].spawn(f)

  def spawnAsync[A](f: ProcessContext => Process[A])(using ProcessContext): Process[A] =
      summon[ProcessContext].spawnAsync(f)

   
  given ProcessCpsMonad = ProcessCpsMonad()

}


object TestProcessLike1 {

  def fun[T:Ordering](t: Int)(using ProcessContext): Process[Unit] = {
    //implicit val printCode = cps.macros.flags.PrintCode 
    //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
    async[Process]{
       t match
         case 0 =>   
         case 1 => 
           val p1: Process[Unit] = ??? //Process.spawn( findFirstInContext3(left, events, p, level+1) )
           //val p2: Process[Unit] = ??? // Process.spawn( findFirstInContext3(right, events, p, level+1) )
           if (t>0) {
             Process.spawn{
               await(p1)
               //await(p2)
               ()
             }
           }
           ???
    }
   }


}
