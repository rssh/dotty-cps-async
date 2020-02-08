package cps

import scala.quoted._
import scala.util.{Try,Success,Failure}
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

trait ComputationBound[T] {
 
}


object ComputationBoundAsyncMonad  {

   def pure[T](value:T): ComputationBound[T] = ???

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = ???

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = ???

}


