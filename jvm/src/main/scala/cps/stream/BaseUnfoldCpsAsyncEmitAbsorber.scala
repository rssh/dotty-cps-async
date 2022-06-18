package cps.stream

import cps.{*,given}

import scala.concurrent.*
import scala.util.*
import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque

trait BaseUnfoldCpsAsyncEmitAbsorber[R,F[_],C<:CpsMonadContext[F],T](using val ec:ExecutionContext, val asyncMonad: CpsConcurrentMonad[F]{ type Context = C}) extends CpsAsyncEmitAbsorber4[R,F,C,T]:

 
   def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): R

   def asSync(fs:F[R]):R

   def eval(f: C  => CpsAsyncEmitter[Monad,Element] => Monad[Unit]): R =
      asSync(evalAsync(f))


   sealed class SupplyEventRecord
   case object SpawnEmitter extends SupplyEventRecord
   case class Emitted(value: T, emitPromise: Promise[Unit]) extends SupplyEventRecord
   case class Finished(result: Try[Unit]) extends SupplyEventRecord

   type ConsumerCallback = Try[SupplyEventRecord]=>Unit
   type OneThreadTaskCallback = Unit => Unit

   class State:
      val finishRef = new AtomicReference[Try[Unit]|Null]()
      val emitStart = new AtomicBoolean()
      val supplyEvents = new ConcurrentLinkedDeque[SupplyEventRecord]()
      val consumerEvents = new ConcurrentLinkedDeque[Promise[SupplyEventRecord]]()
      val stepStage = new AtomicInteger(0)
      final val StageFree = 0
      final val StageBusy = 1
      final val StageContinue = 3

      def queueEmit(v:T): F[Unit] =
         val p = Promise[Unit]()
         val emitted = Emitted(v, p)
         supplyEvents.offer(emitted)
         val retval = asyncMonad.adoptCallbackStyle[Unit]{ emitCallback => 
            p.future.onComplete(emitCallback)
         }
         enterStep()
         retval
         
      def queueConsumer(): F[SupplyEventRecord] =
         val p = Promise[SupplyEventRecord]()
         consumerEvents.offer(p)
         val retval = asyncMonad.adoptCallbackStyle[SupplyEventRecord]{ evalCallback =>
            p.future.onComplete(evalCallback)
         }
         enterStep()
         retval

      def finish(r: Try[Unit]):Unit =
         finishRef.set(r)
         enterStep()
      

      private def enterStep(): Unit = {
         var isSet = false
         while (!isSet) {
            if (stepStage.compareAndSet(StageFree,StageBusy)) then
               isSet = true
               summon[ExecutionContext].execute(() =>step())
            else if (stepStage.compareAndSet(StageBusy,StageContinue)) then
               isSet = true
            else if (stepStage.compareAndSet(StageContinue,StageContinue)) then
               isSet = true
         }
      }

      private def step(): Unit = {
         var done = false
         while(!done) {
            while(!supplyEvents.isEmpty && !consumerEvents.isEmpty) {
               val consumer = consumerEvents.poll()
               if !(consumer == null) then
                  val supply = supplyEvents.poll() 
                  if !(supply == null) then
                     // can we hope the
                     consumer.success(supply)
                  else
                     consumerEvents.addFirst(consumer) 
            }
            checkFinish()
            if supplyEvents.isEmpty() || consumerEvents.isEmpty() then
               if(stepStage.compareAndSet(StageBusy, StageFree)) then
                  if supplyEvents.isEmpty() || consumerEvents.isEmpty() then
                     done = true
                  else
                     enterStep()
               else 
                  stepStage.set(StageBusy)
         }
      }

      private def checkFinish(): Unit = {
         val r = finishRef.get()
         if !(r == null) then
            while(! consumerEvents.isEmpty ) {
               val consumer = consumerEvents.poll()
               if  (consumer != null) then
                  consumer.nn.success(Finished(r.nn))
            }
            while(! supplyEvents.isEmpty) {
               val ev = supplyEvents.poll()
               if (ev != null) then
                  ev match
                     case Emitted(v,p) =>
                        p.failure(new CancellationException("Stream is closed"))
                     case _ =>
            }
      } 


   class StepsObserver(state: State) extends CpsAsyncEmitter[F,T]:
   
   
     def emitAsync(v:T): F[Unit] =  
         if (state.supplyEvents.isEmpty) then
            val consumer = state.consumerEvents.poll()
            if (consumer == null) then
               state.queueEmit(v)
            else      
               val p = Promise[Unit]()
               val emitted = Emitted(v, p)
               consumer.success(emitted)
               asyncMonad.adoptCallbackStyle[Unit]{ emitCallback =>
                  p.future.onComplete(emitCallback)
               }
         else
            state.queueEmit(v)
            

      
     def finish(r: Try[Unit]): Unit =
          state.finish(r)
          
          
   end StepsObserver

    
   def evalAsync(f: C => CpsAsyncEmitter[F,T] => F[Unit]):F[R] =
      asyncMonad.apply( ctx => asyncMonad.pure(evalAsyncInternal(f(ctx))) )


   def evalAsyncInternal(f: CpsAsyncEmitter[F,T] => F[Unit]): R =

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
              case Emitted(v, emitPromise) =>
                     emitPromise.success(())
                     summon[CpsAsyncMonad[F]].pure(Some(v,state))
              case Finished(r) =>
                     r match
                        case Success(_) => summon[CpsAsyncMonad[F]].pure(None)
                        case Failure(e) => summon[CpsAsyncMonad[F]].error(e)               
              case other =>
                     // impossible.
                     asyncMonad.error(new RuntimeException(s"impossible: unexpected event $other")) 

         def nextEvent(): F[SupplyEventRecord] =    
            if (state.emitStart.compareAndSet(false,true)) then
               asyncMonad.pure(SpawnEmitter)
            else 
               state.queueConsumer()
               
         val r = state.finishRef.get()         
         if r == null then
            val e = state.supplyEvents.poll()
            if (e == null) then
               asyncMonad.flatMap(nextEvent())(e => handleEvent(e))
            else
               handleEvent(e)     
         else  
            r match
               case Success(()) => asyncMonad.pure(None)
               case Failure(ex) =>
                  asyncMonad.error(ex)
     
      end step

      unfold(state)(step)
         
   end evalAsyncInternal
