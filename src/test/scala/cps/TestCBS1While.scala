package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Try
import scala.util.Success
import scala.util.Failure


trait ComputationBound[T] {
 
  def map[S](f:T=>S): ComputationBound[S] = ???

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S]

}

implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ??? 

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

}


class TestBS1While

  def cbBool(b:Boolean): ComputationBound[Boolean] = ???

 // Dotty crash.
 // TODO: minimize and submit bug.
 //
  @Test def tWhileC1_11(): Unit = 
     val c = Async.transform[ComputationBound,Unit]{
        while(await(cbBool(false))) {
          await(cbBool(false))
        }
     }
     assert(true)



