package cps.pe

import scala.util._
import scala.concurrent._

import cps._

/**
 * pure effect, which is referential transparent.
 * Needed for emulation of work with 'pure' systems, such as cats-io.
 **/
trait PureEffect[A]  {

   def map[B](f: A=>B): PureEffect[B] = MappedThunk(this, 
      { case Success(x) => Success(f(x))
        case Failure(ex) => Failure(ex)
      })

   def flatMap[B](f: A=>PureEffect[B]): PureEffect[B] = 
      FlatMappedThunk(this, {
         case Success(x) => f(x)
         case Failure(ex) => PureEffect.raiseError(ex)
      })

   def flatMapTry[B](f: Try[A] => PureEffect[B]): PureEffect[B] =
      FlatMappedThunk(this, f)
        
   def memoize(): PureEffect[PureEffect[A]] =
      MappedThunk(this,
         r => Success(TryThunk(()=>r))
      )
      
      

   def unsafeRunFuture()(using ec: ExecutionContext): Future[A] = 
     PureEffect.unsafeRunFuture(this)

}

object PureEffect {

  def pure[T](value: T): PureEffect[T] = TryThunk(()=>Success(value))

  def delay[T](value: =>T): PureEffect[T] = TryThunk(()=>Try(value))

  def adoptFutureRun[T](value: =>Future[T]): PureEffect[T] =
       FutureThunk(() => value)

  def raiseError[T](ex: Throwable): PureEffect[T] = TryThunk(()=>Failure(ex))

  
  def unsafeRunFuture[A](eff: PureEffect[A])(using ec: ExecutionContext): Future[A] =
        eff match 
          case TryThunk(f) => 
                    Future.fromTry(f())
          case FutureThunk(f) => f()
          case MappedThunk(fa,f) =>
                  unsafeRunFuture(fa).transform(f)
          case FlatMappedThunk(fa,f) =>
                  // ty example - recursive without trampoline for now
                  unsafeRunFuture(fa).transformWith( x =>  unsafeRunFuture(f(x)))
                                       
 

}


case class TryThunk[T](fun:()=>Try[T]) extends PureEffect[T]
case class FutureThunk[T](fun:()=>Future[T]) extends PureEffect[T]
case class MappedThunk[A,B](fa: PureEffect[A], f: Try[A] => Try[B]) extends PureEffect[B]
case class FlatMappedThunk[A,B](fa: PureEffect[A], f: Try[A] => PureEffect[B]) extends PureEffect[B]


given PureEffectCpsMonad: CpsConcurrentEffectMonad[PureEffect] with CpsMonadInstanceContext[PureEffect] with

  type F[T] = PureEffect[T]

  type Spawned[A] = Future[A]

  def pure[A](x:A): PureEffect[A] = PureEffect.pure(x)

  def error[A](e: Throwable): PureEffect[A] = PureEffect.raiseError(e)

  def map[A,B](fa: F[A])(f: A=>B): F[B] =
       fa.map(f)

  def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B] =
       fa.flatMap(f)

  def flatMapTry[A,B](fa: F[A])(f: Try[A]=> F[B]): F[B] =
       fa.flatMapTry(f)

  def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A] =
       val p = Promise[A]()
       source{
          case Success(a) => p success a
          case Failure(ex) => p failure ex
       }
       FutureThunk(()=>p.future)

  def spawnEffect[A](op: => PureEffect[A]): PureEffect[Spawned[A]] =
       FutureThunk{() =>
          given ExecutionContext = ExecutionContext.global
          Future(
            PureEffect.unsafeRunFuture(op)
          )
       }

  def join[A](op: Spawned[A]): PureEffect[A] =
       op.value match
         case Some(r) => TryThunk(()=>r)
         case None => FutureThunk(()=>op)

  def tryCancel[A](op: Spawned[A]): F[Unit] =
       error(new UnsupportedOperationException("PureEffect.cancel"))



given CpsMonadMemoization.Pure[PureEffect] with

   def apply[T](ft:PureEffect[T]): PureEffect[PureEffect[T]] =
      ft.memoize()


inline transparent given ValueDiscard[PureEffect[Unit]] = AwaitValueDiscard[PureEffect,Unit]
  

