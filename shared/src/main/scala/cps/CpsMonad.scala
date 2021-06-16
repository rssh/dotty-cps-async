package cps

import scala.quoted._
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicReference


/**
 * Basic CpsMonad operations.
 * Implementing this typeclass is enough to use async/await with supports of
 * basic control-flow constructions (if, loops, but no exceptions).
 **/
trait CpsMonad[F[_]] extends CpsAwaitable[F] {

   type WF[X] = F[X]

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

}


/**
 * Marker typeclass for wrappers, which we can await.
 * Such traits can be not monads itself (for example, its impossible to set monad structure over js.Promise)
 * but can be convertable into cps monads.
 **/
trait CpsAwaitable[F[_]] 


/**
 * If you monad supports this typeclass, than
 * you can use try/catch/finally inside await.
 **/
trait CpsTryMonad[F[_]] extends CpsMonad[F] {

   def error[A](e: Throwable): F[A]

   def flatMapTry[A,B](fa:F[A])(f: Try[A] => F[B]):F[B]

   def mapTry[A,B](fa:F[A])(f: Try[A] => B):F[B] =
       flatMapTry(fa)(x => pure(f(x)))

   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A] =
         flatMapTry[A,A](fa){ x =>
           x match
             case Success(a) => pure(a)
             case Failure(e) => try{
                                  fx(e)
                                }catch{
                                  case NonFatal(ex) => error(ex)
                                }
         }


   def withAction[A](fa:F[A])(action: =>Unit):F[A] =
    flatMap(
       restore(fa){ ex =>
         try {
           action
           error(ex)
         }catch{
           case ex1: Throwable => ex.addSuppressed(ex1)
             error(ex)
         }
       }
      ){x =>
        try{
          action
          pure(x)
        }catch{
          case NonFatal(ex) => error(ex)
        }
      }


   def withAsyncAction[A](fa:F[A])(action: => F[Unit]):F[A] =
    flatMap(
       restore(fa){ ex =>
         flatMap(
           restore(tryImpure(action)){
             ex1 => ex.addSuppressed(ex1)
             error(ex)
           })(_ => error(ex))
       }
    ){ x =>
        map(tryImpure(action))(_ => x)
    }

   // for cps-transform of monad operations.
   def withActionAsync[A](fa:F[A])(action: () => F[Unit]):F[A] =
      withAsyncAction(fa)(action())

   def tryPure[A](a: =>A):F[A] =
       try {
         pure(a)
       } catch {
         //TODO: handle control
         case NonFatal(ex) => error(ex)
       }

   def tryImpure[A](a: =>F[A]):F[A] =
       try {
         a
       } catch {
         case NonFatal(ex) => error(ex)
       }

   def fromTry[A](r: Try[A]): F[A] =
       r match
         case Success(a) => pure(a)
         case Failure(ex) => error(ex)

}


/**
 * Monad, which is compatible with passing data via callbacks.
 *
 * Interoperability with Future:
 * allows  async[F]{ .. await[Future](..) ... }
 **/
trait CpsAsyncMonad[F[_]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediately in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A]

 
}

/**
 * Monad, where we can define a delay effect, as
 * expression, which will be evaluated later,
 **/
trait CpsDelayMonad[F[_]] extends CpsAsyncMonad[F] {

   def delayedUnit:F[Unit]

}


/**
 * Monad, where we can define an effect of starting operation in
 *  different execution flow.
 **/
trait CpsConcurrentMonad[F[_]] extends CpsAsyncMonad[F] {

   /**
    * Spawned[A] is a computation, which is executed in own flow.
    * (i.e. Fiber, Future, etc ..)
    **/
   type Spawned[A]

   /**
    * spawn execution of operation in own execution flow.
    **/
   def spawnEffect[A](op: =>F[A]): F[Spawned[A]]

   def join[A](op: Spawned[A]): F[A]

   def tryCancel[A](op: Spawned[A]): F[Unit]

   /**
    * join two computations in such way, that they will execute concurrently.
    **/
   def concurrently[A,B](fa:F[A], fb:F[B]): F[Either[(Try[A],Spawned[B]),(Spawned[A],Try[B])]] =
       flatMap(spawnEffect(fa)){ sa =>
         flatMap(spawnEffect(fb)){ sb =>
            val ref = new AtomicReference[Either[(Try[A],Spawned[B]),(Spawned[A],Try[B])]|Null](null)
            val endA = adoptCallbackStyle[Either[(Try[A],Spawned[B]),(Spawned[A],Try[B])]]{ callback => 
              mapTry(join(sa)){ ra => 
                 val v = Left(ra,sb)
                 if ref.compareAndSet(null,v) then 
                    callback(Success(v)) 
              }
              mapTry(join(sb)){ rb =>
                 val v = Right(sa, rb)
                 if ref.compareAndSet(null,v) then 
                    callback(Success(v)) 
              }
            }
            endA
         }
       }


}


/**
 * Monad, where we can spawn some event and be sure that one
 * be evaluated, event if we drop result.
 *
 * Interoperability with Future:
 * allows  async[Future]{ .. await[F](..) ... }
 **/
trait CpsSchedulingMonad[F[_]] extends CpsConcurrentMonad[F] {


   /**
    * schedule execution of op somewhere, immediatly.
    * Note, that characteristics of scheduler can vary.
    **/
   def spawn[A](op: =>F[A]): F[A]

   type Spawned[A] = F[A]

   def spawnEffect[A](op: =>F[A]): F[F[A]] =
         pure(spawn(op))

   def join[A](op: Spawned[A]): F[A] = op

         

}


trait CpsFulfillingMonad[F[_]] extends CpsAsyncMonad[F] {

   /**
    * block until monad will be finished or timeout will be expired.
    * Note, that using this operation inside async is dangerous.
    **/
   def fulfill[T](t:F[T], timeout: Duration): Option[Try[T]]

}


object CpsMonad:

  object ForSyntax:

    extension [F[_],T,S](x:F[T])(using m:CpsMonad[F])

      def flatMap(f: T=>F[S]): F[S] =
         m.flatMap(x)(f)

      def map(f: T=>S): F[S] =
         m.map(x)(f)



