package cps

import scala.quoted._
import scala.util.{Try,Success,Failure}

trait ComputationBound[T] {
 
  def run(): Try[T]

  def map[S](f:T=>S): ComputationBound[S] =
     flatMap( x => Done(f(x)) )

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S]

}

object ComputationBound {
   
   def pure[T](value:T): ComputationBound[T] = Done(value)

}

implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ComputationBound.pure(value)

   def finalAwait[T](t:ComputationBound[T]):Try[T] = t.run()

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

   def error[T](e: Throwable):ComputationBound[T] = Error[T](e)

   def suspend[A](thunk: => ComputationBound[A]): ComputationBound[A] = Thunk(() =>thunk)

   def delay[A](thunk: =>A): ComputationBound[A] = Thunk(() => Done(thunk))

}

case class Thunk[T](thunk: ()=>ComputationBound[T]) extends ComputationBound[T] {

  def run(): Try[T] = 
        thunk() match 
           case Done(t) => Success(t)
           case Thunk(f1) => f1().run()
           case Error(e) => Failure(e)
        

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     Thunk[S]{ () => run() match 
                         case Success(t) => f(t)
                         case Failure(e) => Error(e)
             }
     
}

case class Done[T](value:T) extends ComputationBound[T] 

  def run(): Try[T] = Success(value)

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     Thunk( () => f(value) )


case class Error[T](e: Throwable) extends ComputationBound[T] 

  def run(): Try[T] = Failure(e)

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     this.asInstanceOf[ComputationBound[S]]


