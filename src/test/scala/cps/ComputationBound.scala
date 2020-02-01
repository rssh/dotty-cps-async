package cps

import scala.quoted._
import scala.util.{Try,Success,Failure}
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

trait ComputationBound[T] {
 

}


implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ??? 

   def finalAwait[T](t:ComputationBound[T]):Try[T] = ??? 

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = ??? 

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = ??? 
      

   def error[T](e: Throwable):ComputationBound[T] = ??? 

   def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = ???

   def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = ??? 

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit):ComputationBound[A] = ???

   def spawn[A](op: =>ComputationBound[A]): ComputationBound[A] = ???

   def fulfill[T](t: ComputationBound[T], timeout: Duration): Option[Try[T]] = ???

}

