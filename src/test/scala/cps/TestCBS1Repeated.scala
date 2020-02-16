package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Try
import scala.util.Success
import scala.concurrent._ // Duration
import scala.concurrent.duration._ // Duration

trait ComputationBound[T]

implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ???

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = ???

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = ???

   def error[T](e: Throwable):ComputationBound[T] = ???


   def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = ???

   def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = ???

   // TODO: not all interesting monads can adopt callback

   /**
    * return a future, which will be completed after callback will-be
    * called by the source.
    **/
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit):ComputationBound[A] = ???

   def spawn[A](op: =>ComputationBound[A]): ComputationBound[A] = ???

   def fulfill[T](t:ComputationBound[T], timeout: Duration): Option[Try[T]] = ???


}


class TestCBS1Repeated:

  def qqq:Int = 0
  
  @Test def repeated_0(): Unit = 
     val c = Async.transform[ComputationBound,String]{
       s"a ${1} ${2}"
     }
     //assert(c.run() == Success("a 2 4"))


