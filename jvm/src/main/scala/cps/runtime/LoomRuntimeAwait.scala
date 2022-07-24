package cps.runtime

import cps.*
import java.util.concurrent.{CompletableFuture => JCompletableFuture}
import scala.util.*
import scala.util.control.NonFatal


trait LoomRuntimeAwait[F[_]] extends CpsRuntimeAwait[F] {

  def runAsync[A,C <: CpsMonadContext[F]](f: C=>A)(m: CpsAsyncEffectMonad[F], ctx:C):F[A] = {
      val retval = m.adoptCallbackStyle[A]{ listener =>
        submit{
          m.delay{
            Loom.startVirtualThread( () =>
              {
                try
                  val a = f(ctx)
                  listener(Success(a))
                catch
                  case NonFatal(ex) =>
                    listener(Failure(ex))
              }
            )
          }    
        }(m, ctx)
      }
      retval
  }

  def await[A](fa: F[A])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): A = {
     val jcf = JCompletableFuture[A]()
     val wrapped = ctx.adoptAwait(fa)
     submit{ 
         m.mapTry(wrapped){
          case Success(a) => jcf.complete(a)
          case Failure(ex) => jcf.completeExceptionally(ex)
        }
     }(m, ctx)   
     jcf.get().nn
  }
  
  def submit[A](fa: F[A])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): Unit

}