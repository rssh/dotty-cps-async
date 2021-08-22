package cps.stream

import cps.{*,given}

import scala.concurrent.*
import scala.util.*
import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque

trait BaseUnfoldCpsAsyncEmitAbsorber[R,F[_]:CpsConcurrentMonad,T](using ExecutionContext ) extends CpsAsyncEmitAbsorber3[R,F,T]:

   def unfold[S](s0:S)(f:S => F[Option[(T,S)]]): R

   val asyncMonad: CpsConcurrentMonad[F] = summon[CpsConcurrentMonad[F]]

   sealed class SupplyEventRecord
   case object SpawnEmitter extends SupplyEventRecord
   case class Emitted(value: T, emitCallback: Try[Unit]=>Unit) extends SupplyEventRecord
   case class Finished(result: Try[Unit]) extends SupplyEventRecord

   type ConsumerCallback = Try[SupplyEventRecord]=>Unit
   type OneThreadTaskCallback = Unit => Unit

   class State:
      val finishRef = new AtomicReference[Try[Unit]|Null]()
      val emitStart = new AtomicBoolean
      val supplyEvents = new ConcurrentLinkedDeque[SupplyEventRecord]()
      val consumerEvents = new ConcurrentLinkedDeque[ConsumerCallback]()
      val stepStage = new AtomicInteger(0)
      final val StageFree = 0
      final val StageBusy = 1
      final val StageContinue = 3

      def queueEmit(v:T): F[Unit] =
         val p = Promise[Unit]()
         val emitted = Emitted(v, x => p.tryComplete(x) )
         supplyEvents.offer(emitted)
         val retval = asyncMonad.adoptCallbackStyle[Unit]{ emitCallback => 
            p.future.onComplete(emitCallback)
         }
         enterStep()
         retval
         
      def queueConsumer(): F[SupplyEventRecord] =
         val p = Promise[SupplyEventRecord]()
         consumerEvents.offer( x => p.complete(x))
         enterStep()
         asyncMonad.adoptCallbackStyle[SupplyEventRecord]{ evalCallback =>
            p.future.onComplete(evalCallback)
            //consumerEvents.offer(evalCallback)
         }
      

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
               if !(consumer eq null) then
                  val supply = supplyEvents.poll() 
                  if !(supply eq null) then
                     // can we hope the
                     consumer(Success(supply))
                  else
                     consumerEvents.addFirst(consumer) 
            }
            if (stepStage.compareAndSet(StageBusy, StageFree)) then
               done = true
            else 
               stepStage.set(StageBusy)
         }
      }



      
   val unitSuccess = Success(())


   class StepsObserver(state: State
                        ) extends CpsAsyncEmitter[R,F,T]:
   
   
     def emitAsync(v:T): F[Unit] =  

         if (state.supplyEvents.isEmpty) then
            val consumer = state.consumerEvents.poll()
            if (consumer eq null) then
               state.queueEmit(v)
            else      
               asyncMonad.adoptCallbackStyle{ emitCallback =>
                  val emitted = Emitted(v, emitCallback)
                  consumer(Success(emitted))
               }
         else
            state.queueEmit(v)
            

      
     def finish(r: Try[Unit]): Unit =
          state.finishRef.set(r)
          while(! state.consumerEvents.isEmpty ) {
             val consumer = state.consumerEvents.poll()
             if ! (consumer eq null) then
               consumer(Success(Finished(r)))
          }
          while(! state.supplyEvents.isEmpty) {
             val ev = state.supplyEvents.poll()
             if ! (ev eq null) then
               ev match
                  case Emitted(v,cb) =>
                     cb(Failure(new CancellationException("Stream is closed")))
                  case _ =>
          }
          
          
   end StepsObserver



   def evalAsync(f: CpsAsyncEmitter[R,F,T] => F[Unit]): R =

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
              case other =>
                     // impossible.
                     asyncMonad.error(new RuntimeException(s"impossible: unexpected event $other")) 

         def nextEvent(): F[SupplyEventRecord] =    
            if (state.emitStart.compareAndSet(false,true)) then
               asyncMonad.pure(SpawnEmitter)
            else 
               state.queueConsumer()
               
         val r = state.finishRef.get()         
         if r eq null then
            val e = state.supplyEvents.poll()
            if (e eq null) then
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
         
   end evalAsync
