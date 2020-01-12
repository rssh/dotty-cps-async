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

   def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = Thunk(() => {
         fa.run() match 
            case Success(a) => Done(a)
            case Failure(ex) => fx(ex)
       })

   def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = Thunk(() => {
         val r = fa.run()  
         action
         r match {
           case Success(a) => Done(a)
           case Failure(ex) => Error(ex)
         }
       })

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


implicit object ComputationBoundMetaMonadDirect extends AsyncMetaMonad[ComputationBound] {

   def pure[T:Type](t:Expr[T]):(given ctx:QuoteContext) => Expr[ComputationBound[T]] =
    '{ ComputationBound.pure(${t}) }

   def finalAwait[T:Type](t:Expr[ComputationBound[T]]):(given ctx:QuoteContext) => Expr[T] =
     import scala.concurrent.duration._
     '{ ${t}.run() match 
         case Failure(e) => throw e
         case Success(t) => t
      }


   type F = ComputationBound

   def map[A,B](fa:F[A])(f: A=>B):F[B] = ???

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B] = ???

   def suspend[A](thunk: =>F[A]): F[A] = ???

   def delay[A](thunk: =>A): F[A] = ???


}


final val computationBoundMetaMonadDirect = ComputationBoundMetaMonadDirect
