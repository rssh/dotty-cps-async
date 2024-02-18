package logic.unification2


import cps.CpsTryMonad
import logic.{CpsLogicMonad, CpsLogicMonadInstanceContext}

import scala.collection.immutable.LongMap
import scala.collection.immutable.Queue
import scala.util.*
import scala.util.control.NonFatal
import scala.util.boundary.*

import cps.*

sealed trait UniWrapperCP1[F[_]:CpsTryMonad,A] {

  type _R = A

  def flatMapTry[B](f: Try[A] => UniWrapperCP1[F,B]): UniWrapperCP1[F,B]

  /**
   * Called in case of FlatMap is delated and put into FlatMapQueus
   * and then we apply flatMapTry accross all queue whe evaluating.
   *
   * (the same as flatMap fpr early evaluation.)
   */
  def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B]

  def mplus(other: =>UniWrapperCP1[F,A]): UniWrapperCP1[F,A]

  def fsplit: F[Option[(Try[A], UniWrapperCP1[F,A])]]

}



object UniWrapperCP1 {

  case class Zero[F[_]:CpsTryMonad,A]() extends UniWrapperCP1[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      this.asInstanceOf[Zero[F,B]]

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      this.asInstanceOf[Zero[F,B]]

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      other

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class Pure[F[_]:CpsTryMonad, A](a: A) extends UniWrapperCP1[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      f(Success(a))

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      f(Success(a))

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      Cons[F,A](Success(a), Susp(() => other))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(a), Zero[F,A]())))

  }




  case class Cons[F[_]:CpsTryMonad,A](head: Try[A], tail: UniWrapperCP1[F,A]) extends UniWrapperCP1[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      applyFlatMapTryToHead(f).mplus(tail.flatMapTry(f))

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      applyFlatMapTryToHead(f).mplus(tail.flatMapTry(f))

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      summon[CpsTryMonad[F]].pure(Some((head, tail)))

    private final def applyFlatMapTryToHead[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      try
        f(head)
      catch
        case NonFatal(ex) =>
          WaitF[F,B](summon[CpsTryMonad[F]].error(ex))

  }


  case class Susp[F[_]:CpsTryMonad,A](susp: () => UniWrapperCP1[F,A]) extends UniWrapperCP1[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      // TODO: use type-aligned queue or trampolined function
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapperCP1[F,?]]))

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      val next = summon[CpsTryMonad[F]].map(susp().flatMapTry(f).fsplit){
        case None => Zero[F,B]()
        case Some((head,tail)) => Cons[F,B](head,tail)
      }
      WaitF(next)

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      susp().fsplit



  }


  case class FlatMapQueue[F[_]:CpsTryMonad,A,R](start: UniWrapperCP1[F,A], queue: Queue[Try[?] => UniWrapperCP1[F,?]]) extends UniWrapperCP1[F,R] {

    override def flatMapTry[B](f: Try[R] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      FlatMapQueue[F,A,B](start, queue.enqueue(f.asInstanceOf[Try[?] => UniWrapperCP1[F,?]]))

    override def applyFlatMapTry[B](f: Try[R] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      applyFlatMaps().applyFlatMapTry(f)

    override def mplus(other: => UniWrapperCP1[F, R]): UniWrapperCP1[F, R] =
      MPlusQueue[F,R](Queue(this,Susp(()=>other)))


    override def fsplit: F[Option[(Try[R], UniWrapperCP1[F, R])]] =
      summon[CpsTryMonad[F]].flatMap(start.fsplit) {
        case None => summon[CpsTryMonad[F]].pure(None)
        case Some((startHead,startTail)) =>
          val startHeadUniWrapperCP1 = FlatMapQueue[F,A,R](fromTry(startHead), queue).applyFlatMaps()
          summon[CpsTryMonad[F]].flatMap(startHeadUniWrapperCP1.fsplit) {
            case None => FlatMapQueue(startTail, queue).fsplit
            case Some((startHeadApplyHead, startHeadApplyTail)) =>
              val next = Susp(() => startHeadApplyTail mplus FlatMapQueue(startTail, queue))
              summon[CpsTryMonad[F]].pure(Some((startHeadApplyHead, next)))
          }
      }


    def applyFlatMaps(): UniWrapperCP1[F,R] =
      val s0: UniWrapperCP1[F,?] = start.asInstanceOf[UniWrapperCP1[F,?]]
      val r = queue.foldLeft(s0){ (s,e) =>
        s.applyFlatMapTry(e.asInstanceOf[Try[s._R] => UniWrapperCP1[F,Any]])
      }
      r.asInstanceOf[UniWrapperCP1[F,R]]

  }



  case class WaitF[F[_]:CpsTryMonad,A](fa: F[UniWrapperCP1[F,A]]) extends UniWrapperCP1[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      WaitF(
        summon[CpsTryMonad[F]].map(fa)(_.flatMapTry(f))
      )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      WaitF(
        summon[CpsTryMonad[F]].map(fa)(_.applyFlatMapTry(f))
      )

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))


    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      summon[CpsTryMonad[F]].flatMap(fa)(_.fsplit)


  }


  case class MPlusQueue[F[_]:CpsTryMonad,A](queue: Queue[UniWrapperCP1[F,A]]) extends UniWrapperCP1[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      if (queue.isEmpty)
        Zero[F,B]()
      else
        MPlusQueue(queue.map(_.flatMapTry(f)))

    /**
     * BFS - apply only head
     * @param f
     * @tparam B
     * @return
     */
    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      queue.dequeueOption match
        case None => Zero[F,B]()
        case Some((head,tail)) =>
          head.applyFlatMapTry(f)  match
            case Zero() =>
              if (tail.isEmpty)
                Zero[F,B]()
              else
                MPlusQueue(tail).applyFlatMapTry(f)
            case other => other.mplus(MPlusQueue(tail).flatMapTry(f))


    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](queue.enqueue(other))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      queue.dequeueOption match
        case None => summon[CpsTryMonad[F]].pure(None)
        case Some((head,tail)) =>
          summon[CpsTryMonad[F]].flatMap(head.fsplit) {
            case None => MPlusQueue(tail).fsplit
            case Some((headHead,headTail)) =>
              val next = Susp(() => headTail mplus MPlusQueue(tail))
              summon[CpsTryMonad[F]].pure(Some((headHead, next)))
          }


  }

  case class LVNew[F[_]:CpsTryMonad,A](lv: LogicalVariable[A]) extends UniWrapperCP1[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapperCP1[F,?]]) )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      // lv can participate in the rerursie expression
      LTerm[F,A](lv,LongMap.empty).applyFlatMapTry(f)

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class LVBind[F[_]:CpsTryMonad,A](lv: LogicalVariable[A], v: UniWrapperCP1[F,A]) extends UniWrapperCP1[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapperCP1[F,?]]) )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] = {
      toLTerm().applyFlatMapTry(f)
    }

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] =
      toLTerm().fsplit

    def toLTerm(): LTerm[F,A] =
      LTerm[F,A](lv,LongMap(lv.id -> LVarBingingRecord(lv,v)))

  }



  case class LVarBingingRecord[F[_],T](lv: LogicalVariable[T], v: UniWrapperCP1[F,T]) {
    type Tp = T
  }

  case class SplittedLVarBingingRecord[F[_],T](lv: LogicalVariable[T],
                                               currentValue: Option[Try[T]],
                                               next: UniWrapperCP1[F,T])


  case class LTerm[F[_]:CpsTryMonad,A](t: TypedLogicalTerm[A], bindings: LongMap[LVarBingingRecord[F,?]]) extends UniWrapperCP1[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapperCP1[F,?]]) )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapperCP1[F, B]): UniWrapperCP1[F, B] =
      WaitF(summon[CpsTryMonad[F]].map(fsplit){
        case None => Zero[F,B]()
        case Some((head,tail)) =>
          val resHead = try {
            f(head)
          } catch {
            case NonFatal(ex) =>
              WaitF[F, B](summon[CpsTryMonad[F]].error(ex))
          }
          resHead.mplus(tail.flatMapTry(f))
      })

    override def mplus(other: => UniWrapperCP1[F, A]): UniWrapperCP1[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapperCP1[F, A])]] = reify[F]{
      val optNext = reflect(fsplitInBinding(t, bindings))
      optNext match
        case None => None
        case Some((head, nextBindings)) =>
          head match
            case Success(t) =>
              Some((Success(t), buildNextBindings(nextBindings)))
            case Failure(ex) =>
              Some((Failure(ex), buildNextBindings(nextBindings)))
    }



    private def fsplitInBinding[A](t: TypedLogicalTerm[A],
                                   oldBinding: LongMap[LVarBingingRecord[F,?]],
                                   changedBinding: LongMap[SplittedLVarBingingRecord[F,?]] = LongMap.empty
                                  ): F[Option[(Try[A], LongMap[SplittedLVarBingingRecord[F,?]])]] = {

      def getVarBinding(id: Long, currentBinding: LongMap[SplittedLVarBingingRecord[F,?]]):
      F[(Option[Try[?]],LongMap[SplittedLVarBingingRecord[F,?]])] = {
        currentBinding.get(id) match
          case Some(r)  => summon[CpsTryMonad[F]].pure((r.currentValue, currentBinding))
          case None => oldBinding.get(id) match
            case None => summon[CpsTryMonad[F]].error(new RuntimeException(s"unbound variable $id"))
            case Some(LVarBingingRecord(lv,v)) =>
              summon[CpsTryMonad[F]].flatMap(v.fsplit) {
                case None =>
                  val nextBinding = currentBinding.updated(id, SplittedLVarBingingRecord(lv,None,Zero[F,lv.Type]()))
                  summon[CpsTryMonad[F]].pure((None, nextBinding))
                case Some((head,tail)) =>
                  val nextBinding = currentBinding.updated(id, SplittedLVarBingingRecord(lv,Some(head),tail))
                  summon[CpsTryMonad[F]].pure((Some(head), nextBinding))
              }
      }


      def fetchTerm(term:LogicalTerm, currentBinding: LongMap[SplittedLVarBingingRecord[F,?]]):
      F[(Option[Try[LogicalTerm]],LongMap[SplittedLVarBingingRecord[F,?]])] = {
        term match
          case l: LogicalVariable[?] =>
            summon[CpsTryMonad[F]].map(getVarBinding(l.id, currentBinding)) {
              case (optTryA, nextBinding) =>
                val optTerm: Option[Try[LogicalTerm]] = optTryA.map(_.map(a =>
                  LogicalConstant[l.Type](a.asInstanceOf[l.Type])(using l.symbol)
                ))
                (optTerm, nextBinding)
            }
            /*
            here is a problem, compiler error in 3.3.3
            reify[F] {
              val (optTryA, nextBinding) = reflect(getVarBinding(l.id, currentBinding))
              val optTerm: Option[Try[LogicalTerm]] = optTryA.map(_.map(a =>
                LogicalConstant[l.Type](a.asInstanceOf[l.Type])(using l.symbol)
              ))
              (optTerm, nextBinding)
            }

             */
          case l: LogicalConstant[?] =>
            summon[CpsTryMonad[F]].pure(Some(Success(l:LogicalTerm)), currentBinding)
          case l: LogicalFunctionalTerm[t] =>
            val s0: (IndexedSeq[LogicalTerm], LongMap[SplittedLVarBingingRecord[F, ?]]) = (IndexedSeq.empty, currentBinding)
            reify[F] {
              boundary[(Option[Try[LogicalTerm]], LongMap[SplittedLVarBingingRecord[F, ?]])] {
                val s = l.args.foldLeft(s0) { (s, e) =>
                  val (args, currentBinding) = s
                  val (optTryArg, nextBinding) = reflect(fetchTerm(e, currentBinding))
                  val s1 = optTryArg match
                    case None =>
                      break((None, nextBinding))
                    case Some(tryArg) =>
                      tryArg match
                        case Failure(ex) =>
                          break((Some(Failure(ex):Try[LogicalTerm]), currentBinding))
                        case Success(arg) =>
                          (args :+ arg) -> nextBinding
                  s1
                }
                val (nextArgs, nextBinding) = s
                val lt: LogicalTerm = LogicalFunctionalTerm(nextArgs)(using l.symbol)
                // error if we put this expression in pair
                val retval: Option[Try[LogicalTerm]] = Some(Success(lt))
                (retval, nextBinding)
              }
            }



      }

      reify[F] {
        val (optTryTerm, bindings) = reflect(fetchTerm(t, changedBinding))
        val optTryA = optTryTerm.map(_.map(term => 
          term.symbol.fromTerm(term).getOrElse(
            throw new IllegalStateException(s"Term is not fully constructed  ($term) ")
          ).asInstanceOf[A]
        ))
        optTryA.map(tryA => (tryA, bindings))
      }


    }

    private def buildNextBindings(nextBindings: LongMap[SplittedLVarBingingRecord[F,?]]): UniWrapperCP1[F,A] =
      ???


  }

  def fromTry[F[_]:CpsTryMonad,A](t: Try[A]): UniWrapperCP1[F,A] =
    t match
      case Success(a) => Pure[F,A](a)
      case Failure(ex) => WaitF(summon[CpsTryMonad[F]].error(ex))

  def error[F[_]:CpsTryMonad,A](msg: String): UniWrapperCP1[F,A] =
    WaitF(summon[CpsTryMonad[F]].error(new RuntimeException(msg)))

  class UniWrapperCP1Environment[ F[_]:CpsTryMonad] extends UnificationEnvironment[[T]=>>UniWrapperCP1[F,T]] {

    thisUnificationEnvironment =>

    class LContext extends UnificationEnvironmentContext[[T]=>>UniWrapperCP1[F,T]] {
      override def monad: UnificationEnvironment[[T] =>> UniWrapperCP1[F, T]] = thisUnificationEnvironment
    }

    override type Context = LContext

    override type Observer[X] = F[X]

    def pure[A](a:A): UniWrapperCP1[F,A] =
      Pure[F,A](a)

    def map[A,B](fa: UniWrapperCP1[F,A])(f: A=>B): UniWrapperCP1[F,B] = ???

    def flatMap[A,B](fa: UniWrapperCP1[F,A])(f: A=>UniWrapperCP1[F,B]): UniWrapperCP1[F,B] = ???

    def flatMapTry[A,B](fa: UniWrapperCP1[F,A])(f: Try[A]=>UniWrapperCP1[F,B]): UniWrapperCP1[F,B] =
      fa.flatMapTry(f)

    override def error[A](e: Throwable): UniWrapperCP1[F, A] = ???

    override def mzero[A]: UniWrapperCP1[F, A] = ???

    override def mplus[A](x: UniWrapperCP1[F, A], y: =>UniWrapperCP1[F, A]): UniWrapperCP1[F, A] = ???

    override def apply[T](op: LContext => UniWrapperCP1[F, T]): UniWrapperCP1[F, T] = {
      op(new LContext)
    }

    override def msplit[A](c: UniWrapperCP1[F, A]): UniWrapperCP1[F, Option[(Try[A], UniWrapperCP1[F, A])]] = ???

    override def observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def mObserveOne[A](ma: UniWrapperCP1[F, A]): F[Option[A]] = ???

    override def mFoldLeft[A, B](ma: UniWrapperCP1[F, A], zero: F[B])(op: (F[B], F[A]) => F[B]): F[B] = ???

    override def mFoldLeftN[A, B](ma: UniWrapperCP1[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = ???

    //TODO: change to bind with M[A] instead of term
    override def bindTerm[A:Unifiable](lv: LogicalVariable[A], t: TypedLogicalTerm[A]): UniWrapperCP1[F, A] = ???

    override def bind[A:Unifiable](lv: LogicalVariable[A], v: UniWrapperCP1[F, A]): UniWrapperCP1[F, A] = ???

    override def fromTerm[A: Unifiable](t: TypedLogicalTerm[A]): UniWrapperCP1[F, A] = ???

    override def toTerm[A: Unifiable](ma: UniWrapperCP1[F, A]): UniWrapperCP1[F, TypedLogicalTerm[A]] = ???


    def cons[T](head: Try[T], tail: UniWrapperCP1[F,T]): UniWrapperCP1[F,T] =
      Cons[F,T](head,tail)

    def susp[T](susp: () => UniWrapperCP1[F,T]): UniWrapperCP1[F,T] =
      Susp[F,T](susp)

    def waitF[T](fa: F[UniWrapperCP1[F,T]]): UniWrapperCP1[F,T] =
      WaitF[F,T](fa)

    def mplusQueue[T](queue: Queue[UniWrapperCP1[F,T]]): UniWrapperCP1[F,T] =
      MPlusQueue[F,T](queue)

    def lvBind[T](lv: LogicalVariable[T], v: UniWrapperCP1[F,T]): UniWrapperCP1[F,T] =
      LVBind[F,T](lv,v)

    def lTerm[T](t: TypedLogicalTerm[T], bindings: LongMap[LVarBingingRecord[F,?]]): UniWrapperCP1[F,T] =
      LTerm[F,T](t,bindings)

  }


}

