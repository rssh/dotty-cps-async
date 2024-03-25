package cps

import cps.runtime.*

import java.util.concurrent.CompletableFuture
import scala.util.*
import scala.util.control.NonFatal
import scala.concurrent.blocking


object ComputationBoundRuntimeAwait extends LoomRuntimeAwait[ComputationBound] {



  def runAsync[A, C <: CpsTryMonadContext[ComputationBound]](f: C => A)(m: CpsAsyncEffectMonad[ComputationBound], ctx: C): ComputationBound[A] = {
     val waiter = ComputationBound.spawn{
        try {
          val r = f(ctx)
          Done(r)
        } catch
          case  NonFatal(ex) =>
            Error(ex)
     }
     Thread.startVirtualThread( () => waiter.run() )
     waiter
  }

  def await[A](fa: ComputationBound[A])(ctx: CpsTryMonadContext[ComputationBound]): A = {
     fa match
       case Done(a) => a
       case Error(ex) => throw ex
       case _ =>
         val jcf = CompletableFuture[A]()
         val r = ctx.monad.mapTry(fa){
           case Success(a) =>
             val _ = jcf.complete(a)
           case Failure(ex) =>
             val _ = jcf.completeExceptionally(ex)
         }
         Thread.startVirtualThread(
            () => {
              try {
                r.run()
              }catch{
                case ex: Throwable =>
                  jcf.completeExceptionally(ex)
              }
            }
         )
         blocking {
           jcf.get()
         }
  }



}
