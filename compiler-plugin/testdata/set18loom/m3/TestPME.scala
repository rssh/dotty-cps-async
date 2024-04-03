package cpsloomtest

import cps.*

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.annotation.experimental
import scala.util.*
import scala.collection.mutable.{Map, Queue}
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, BlockContext, CanAwait, ExecutionException, blocking}
import org.junit.{Ignore, Test}

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.util.control.NonFatal

sealed trait PoorManEffect[+T]


object PoorManEffect {

  // low-level unsafe internal API
  trait RunAPI {
    def submitAndForget[T](pe: PoorManEffect[T]): Unit
    def submit[T](pe: PoorManEffect[T]): Long
    def checkSubmitted[T](submitId: Long): Option[Try[T]]
    def forgetSubmitted(submitId: Long): Unit
    def listenSubmitted[T](submitId: Long): CompletableFuture[T]
  }

  case class Pure[T](t: T) extends PoorManEffect[T]

  case class Error(e: Throwable) extends PoorManEffect[Nothing]

  case class Thunk[T](th: (RunAPI => PoorManEffect[T])) extends PoorManEffect[T]

  object CpsPMEMonad extends CpsTryMonad[PoorManEffect] with CpsTryMonadInstanceContext[PoorManEffect]  {

    def pure[T](t: T): PoorManEffect[T] = Pure(t)

    def error[T](e: Throwable): PoorManEffect[T] = Error(e)

    def map[A, B](fa: PoorManEffect[A])(f: A => B): PoorManEffect[B] = fa match
      case Pure(t) =>
        inTry(Pure(f(t)))
      case Error(e) => Error(e)
      case Thunk(th) => Thunk((d) => map(inTry(th(d)))(f))

    def flatMap[A, B](fa: PoorManEffect[A])(f: A => PoorManEffect[B]): PoorManEffect[B] = fa match
      case Pure(t) =>
        inTry(f(t))
      case Error(e) => Error(e)
      case Thunk(th) => Thunk(d => flatMap(inTry(th(d)))(f))

    def flatMapTry[A, B](fa: PoorManEffect[A])(f: Try[A] => PoorManEffect[B]): PoorManEffect[B] =
      fa match
        case Pure(t) =>
          inTry(f(Success(t)))
        case Error(e) =>
          inTry(f(Failure(e)))
        case Thunk(th) =>
          Thunk(d => flatMapTry(inTry(th(d)))(f))

    def inTry[X](op: =>PoorManEffect[X]): PoorManEffect[X] =
      try
        op
      catch
        case NonFatal(ex) =>
          Error(ex)

  }

  given CpsPMEMonad.type = CpsPMEMonad


  private class Runner[A] extends RunAPI  {

    case class EvalRecord(pe: PoorManEffect[Any], id: Long)

    // only one thread at a time.
    //val runQueue: Queue[EvalRecord] = Queue()
    val runQueue: java.util.concurrent.ConcurrentLinkedDeque[EvalRecord] = new java.util.concurrent.ConcurrentLinkedDeque()

    val currentWaitId = new AtomicLong(0L)
    val waiters: TrieMap[Long,CompletableFuture[Any]] = TrieMap()
    val processEntryCounter = new AtomicInteger(0)
    val nThunksInProcess = new AtomicInteger(0)
    val submitWaiter = new AnyRef()

    override def submitAndForget[T](pe: PoorManEffect[T]): Unit = {
      //runQueue.enqueue(EvalRecord(pe,nextId)
      runQueue.add(EvalRecord(pe,nextId))
      submitWaiter.synchronized {
        submitWaiter.notifyAll()
      }
    }

    override def submit[T](pe: PoorManEffect[T]): Long =  {
      val id = nextId
      //runQueue.enqueue(EvalRecord(pe,id))
      runQueue.add(EvalRecord(pe,id))
      waiters(id) = new CompletableFuture[Any]()
      submitWaiter.synchronized {
        submitWaiter.notifyAll()
      }
      id
    }

    override def checkSubmitted[T](submitId: Long): Option[Try[T]] = {
        waiters.get(submitId).flatMap{ v =>
          if (v.isDone) {
             Some(Try(v.get().asInstanceOf[T]))
          } else {
             None
          }
        }
    }

    override def forgetSubmitted(submitId: Long): Unit = {
        waiters.remove(submitId)
    }

    override def listenSubmitted[T](submitId: Long): CompletableFuture[T] = {
      waiters.get(submitId) match
        case Some(cf) => cf.asInstanceOf[CompletableFuture[T]]
        case None =>
         throw new IllegalArgumentException(s"invalid submitId=${submitId}")
    }

    private def nextId: Long = {
      currentWaitId.incrementAndGet()
    }

    private def setWaiterResult(id: Long, value: Try[Any]): Unit = {
      waiters.get(id) match
        case Some(cf) =>
          value match
            case Success(t) => cf.complete(t)
            case Failure(e) => cf.completeExceptionally(e)
        case None =>
            throw new IllegalArgumentException(s"invalid submitId=${id}")
    }


    def process(): Unit = {
      //println(s"starting process thread ${Thread.currentThread().getId()}")
      @volatile var blocked: Boolean = false
      @volatile var finished: Boolean = false
      processEntryCounter.incrementAndGet()
      BlockContext.withBlockContext(
        new BlockContext {
          override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
            if (permission == null) {
              throw new NullPointerException("null permission")
            }
            if (!blocked) {
              blocked = true
              val nextThread = Thread.startVirtualThread {
                () => {
                  process()
                }
              }
            }
            val retval = try {
              thunk
            } finally {
              submitWaiter.synchronized {
                submitWaiter.notifyAll()
              }
            }
            retval
          }
        }) {
        while (!finished && !blocked) {
          while(runQueue.isEmpty && !blocked && nThunksInProcess.get() > 0) {
            submitWaiter.synchronized {
                if (runQueue.isEmpty && !blocked && nThunksInProcess.get() > 0) {
                   submitWaiter.wait()
                }
            }
          }
          while (!runQueue.isEmpty && !blocked) {
            //val v = runQueue.dequeue()
            val v = runQueue.poll()
            if (v != null) {
              //
              v.pe match
                case Pure(t) => setWaiterResult(v.id, Success(t))
                case Error(e) =>
                  setWaiterResult(v.id, Failure(e))
                case Thunk(th) =>
                  // here we can have call of block-context.
                  nThunksInProcess.incrementAndGet()
                  try {
                    val r = try {
                      th(this)
                    } catch {
                      case NonFatal(ex) =>
                        Error(ex)
                    }
                    // execution can be moved to be processed in the other virtual thread.
                    runQueue.add(EvalRecord(r, v.id))
                    submitWaiter.synchronized {
                      submitWaiter.notifyAll()
                    }
                  } finally {
                    nThunksInProcess.decrementAndGet()
                  }
            }
          }
          if (!blocked) {
            if (runQueue.isEmpty && nThunksInProcess.get() == 0) {
              finished = true
            }
          }
        }
        processEntryCounter.decrementAndGet()
        //println(s"exiting process thread ${Thread.currentThread().getId()} blocked=${blocked}")
      }

    }




  }



  def run[A](pe: PoorManEffect[A]): A = {
      val runner = new Runner()
      val id0 = runner.submit[A](pe)
      val resultFuture = runner.listenSubmitted[A](id0)
      if (resultFuture.isDone) then
        try
          resultFuture.get()
        catch
          case ex: ExecutionException =>
            throw ex.getCause()
      else
        try
         runner.process()
        catch
          case NonFatal(ex) =>
            println(s"Impossible, excwption from process: ${ex.getMessage}")
            throw ex
        runner.checkSubmitted[A](id0) match
          case Some(Success(t)) => t.asInstanceOf[A]
          case Some(Failure(e)) => throw e
          case None =>
             //in real life we also will think about timeouts.
            if (runner.processEntryCounter.get() == 0)
               // recheck for second thread deliverd result.
              runner.checkSubmitted[A](id0) match
                case Some(Success(t)) => t.asInstanceOf[A]
                case Some(Failure(e)) => throw e
                case None =>
                     println(s"runner.nProcessEntries == ${runner.processEntryCounter.get()}")
                     println(s"runner.runQueue.isEmpty == ${runner.runQueue.isEmpty}")
                     throw new RuntimeException(s"process finished, but no result for id ${id0}")
            else
               blocking {
                 try
                    resultFuture.get()
                 catch
                    case ex: ExecutionException =>
                      throw ex.getCause()
               }

  }

  def runToTry[A](pe: PoorManEffect[A]): Try[A] =
    Try(run(pe))



}

class PoorManEffectRuntimeAwait(rt:PoorManEffect.RunAPI) extends CpsRuntimeAwait[PoorManEffect] {

  //override def runAsync[A, C <: CpsTryMonadContext[PoorManEffect]](f: C => A)(m: CpsAsyncEffectMonad[PoorManEffect], ctx: C): PoorManEffect[A] = {
  //  PoorManEffect.Thunk(r => m.pure(f(ctx)) )
  //}

  override def await[A](fa: PoorManEffect[A])(ctx: CpsTryMonadContext[PoorManEffect]): A = {
    val id = rt.submit(fa)
    val cf = rt.listenSubmitted[A](id)
    // here execution of main loop of runner.process will be moved to other virtual thread.
    blocking{
      val retval = try
                      cf.get()
                   catch
                     case  ex: ExecutionException =>
                       throw ex.getCause()
                   finally
                      rt.forgetSubmitted(id)
      retval
    }
  }

}

given CpsRuntimeAwaitProvider[PoorManEffect] with {


  override def withRuntimeAwait[A](lambda: CpsRuntimeAwait[PoorManEffect] => PoorManEffect[A])(using ctx: CpsTryMonadContext[PoorManEffect]): PoorManEffect[A] = {
    PoorManEffect.Thunk(rt => lambda(PoorManEffectRuntimeAwait(rt)))
  }


}

@experimental
class TestPME {

  def incr(x:Int): PoorManEffect[Int] = async[PoorManEffect] {
    x+1
  }

  def runPoorManEffectList()(using CpsDirect[PoorManEffect]): List[Int] = {
    val l = List(1, 2, 3)
    val l2 = l.map(x => await(incr(x)))
    l2
  }

  @Test
  def testPoorManEffectList(): Unit = {
    val pe: PoorManEffect[List[Int]] = async[PoorManEffect] {
      runPoorManEffectList()
    }
    val r = PoorManEffect.run[List[Int]](pe)
    assert (r == List(2,3,4))
  }

  def runPoorManEffectMyList()(using CpsDirect[PoorManEffect]): MyList[Int] = {
    val l = MyList.create(1, 2, 3)
    val l2 = l.map(x => await(incr(x)))
    l2
  }

  @Test
  def testPoorManEffectMyList(): Unit = {
    val pe = async[PoorManEffect] {
      runPoorManEffectMyList()
    }
    val r = PoorManEffect.run(pe)
    assert(r == MyList.create(2, 3, 4))
  }

  def twice[A](f: A=>A)(arg:A):A = {
    f(f(arg))
  }

  def m2(x:Int):PoorManEffect[Int] =
    PoorManEffect.Thunk {
      rt =>
        PoorManEffect.Pure(x * 2)
    }

  def runPMETwice()(using CpsDirect[PoorManEffect]): Int = {
    val l = 124
    val l2 = twice[Int](x => await(m2(x)))(124)
    l2
  }

  @Test
  def testPMETwice(): Unit = {
    val pe = async[PoorManEffect] {
      runPMETwice()
    }
    val r = PoorManEffect.run(pe)
    assert(r == 124*4)
  }

  def runPMECatchExceptionFromAwait(using CpsDirect[PoorManEffect]): Int = {
    val list0 = MyList.create(1, 2, 3, 4, 5)
    try {
      val list1 = list0.map[Int](x => await(PoorManEffect.Error(new RuntimeException("test"))))
      org.junit.Assert.assertTrue(false)
      1
    } catch {
      case ex: RuntimeException =>
        //dotty error durint -Ycheck:all
        //  TODO: create test-case with issue
        //assert(ex.getMessage() == "test")
        org.junit.Assert.assertTrue(ex.getMessage() == "test")
        2
    }
  }

  @Test
  def testPMECatchExceptionFromAwait(): Unit = {
    val c = async[PoorManEffect] {
      runPMECatchExceptionFromAwait
    }
    val r = PoorManEffect.run(c)
    assert(r==2)
  }


}
