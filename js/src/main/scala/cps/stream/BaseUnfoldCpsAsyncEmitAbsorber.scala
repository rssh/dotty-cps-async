package cps.stream

import cps.{*,given}

import scala.concurrent.*
import scala.util.*
import scala.collection.mutable.Queue
import java.util.concurrent.atomic.*



trait BaseUnfoldCpsAsyncEmitAbsorber[R,F[_],C<:CpsMonadContext[F], T](using ec: ExecutionContext, auxAsyncMonad: CpsConcurrentMonad.Aux[F,C] ) extends CpsAsyncEmitAbsorber4[R,F,C,T]:

  def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): R

  def asSync(fs:F[R]):R

  def eval(f: C  => CpsAsyncEmitter[Monad,Element] => Monad[Unit]): R =
      asSync(evalAsync(f))


  val asyncMonad: CpsConcurrentMonad.Aux[F,C] = auxAsyncMonad

  sealed class SupplyEventRecord
  case object SpawnEmitter extends SupplyEventRecord
  case class Emitted(value: T, emitCallback: Try[Unit]=>Unit) extends SupplyEventRecord
  case class Finished(result: Try[Unit]) extends SupplyEventRecord

  type ConsumerCallback = Try[SupplyEventRecord]=>Unit
  type OneThreadTaskCallback = Unit => Unit

  class State:
    val finishRef = new AtomicReference[Try[Unit]|Null]()
    val emitStart = new AtomicBoolean()
    val supplyEvents = Queue[SupplyEventRecord]()
    val consumerEvents = Queue[ConsumerCallback]()
 
    def queueEmit(v:T): F[Unit] =
      val p = Promise[Unit]()
      val emitted = Emitted(v, x => p.tryComplete(x) )
      supplyEvents.enqueue(emitted)
      asyncMonad.adoptCallbackStyle{ emitCallback => 
         p.future.onComplete(emitCallback)
      }
      
    def queueConsumer(): F[SupplyEventRecord] =
      val p = Promise[SupplyEventRecord]()
      consumerEvents.enqueue( x => p.complete(x))
      asyncMonad.adoptCallbackStyle[SupplyEventRecord]{ evalCallback =>
         p.future.onComplete(evalCallback)
      }
      
    
  val unitSuccess = Success(())


  class StepsObserver(state: State
                     ) extends CpsAsyncEmitter[F,T]:


    def emitAsync(v:T): F[Unit] =  
      if (!state.consumerEvents.isEmpty) then
        val consumer = state.consumerEvents.dequeue()
        asyncMonad.adoptCallbackStyle{ emitCallback =>
          val emitted = Emitted(v, emitCallback)
          consumer(Success(emitted))
        }
      else  
        state.queueEmit(v)
         
    def finish(r: Try[Unit]): Unit =
       state.finishRef.set(r)
       while(! state.consumerEvents.isEmpty ) {
          val consumer = state.consumerEvents.dequeue()
          consumer(Success(Finished(r)))
       }
       while(! state.supplyEvents.isEmpty) {
          val ev = state.supplyEvents.dequeue()
          ev match
              case Emitted(v,cb) =>
                  cb(Failure(new CancellationException("Stream is closed")))
              case _ =>
       }  
       
  end StepsObserver

  def evalAsync(f: C => CpsAsyncEmitter[F,T] => F[Unit]):F[R] =
   asyncMonad.apply( ctx => asyncMonad.pure(evalAsyncInContext(f(ctx))) )


  def evalAsyncInContext(f: CpsAsyncEmitter[F,T] => F[Unit]): R =

    val state = new State()
    val stepsObserver = new StepsObserver(state)
  
    def step(state:State):F[Option[(T,State)]] =

      def handleEvent(e: SupplyEventRecord): F[Option[(T, State)]] =

         e match
           case SpawnEmitter =>
                  val emitter = asyncMonad.spawnEffect{
                        asyncMonad.mapTry(f(stepsObserver)){ r => stepsObserver.finish(r) }
                  }
                  asyncMonad.flatMap(emitter){ _ =>
                     asyncMonad.flatMap(nextEvent()){ e =>
                        handleEvent(e)
                     } 
                  }
           case Emitted(v, emitCallback) =>
                  emitCallback(unitSuccess)
                  summon[CpsAsyncMonad[F]].pure(Some(v,state))
           case Finished(r) =>
                  r match
                     case Success(_) => summon[CpsAsyncMonad[F]].pure(None)
                     case Failure(e) => summon[CpsAsyncMonad[F]].error(e)               

      def nextEvent(): F[SupplyEventRecord] =    
         if (state.emitStart.compareAndSet(false,true)) then
            asyncMonad.pure(SpawnEmitter)
         else 
            if !state.supplyEvents.isEmpty then
              val e = state.supplyEvents.dequeue()
              asyncMonad.pure(e)
            else
              state.queueConsumer() 
            
      val r = state.finishRef.get()         
      if r eq null then
        asyncMonad.flatMap(nextEvent())(e => handleEvent(e))     
      else  
         r match
            case Success(()) => asyncMonad.pure(None)
            case Failure(ex) =>
               asyncMonad.error(ex)
  
    end step

    unfold(state)(step)
      
  end evalAsyncInContext


