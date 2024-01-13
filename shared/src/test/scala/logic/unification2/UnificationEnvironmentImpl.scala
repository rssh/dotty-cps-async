package logic.unification2

import cps.CpsTryMonad
import logic.unification1.LogicalFunctionSymbol
import logic.{CpsLogicMonad, CpsLogicMonadInstanceContext}

import scala.collection.immutable.LongMap
import scala.collection.immutable.Queue
import scala.util.*
import scala.util.control.NonFatal
import scala.util.boundary.*

import cps.*

sealed trait UniWrapper[F[_]:CpsTryMonad,A] {

   type _R = A

   def flatMapTry[B](f: Try[A] => UniWrapper[F,B]): UniWrapper[F,B]

  /**
   * Called in case of FlatMap is delated and put into FlatMapQueus
   * and then we apply flatMapTry accross all queue whe evaluating.
   *
   * (the same as flatMap fpr early evaluation.)
   */
   def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B]

   def mplus(other: =>UniWrapper[F,A]): UniWrapper[F,A]

   def fsplit: F[Option[(Try[A], UniWrapper[F,A])]]

}



object UniWrapper {

  case class Zero[F[_]:CpsTryMonad,A]() extends UniWrapper[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      this.asInstanceOf[Zero[F,B]]

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      this.asInstanceOf[Zero[F,B]]

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      other

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class Pure[F[_]:CpsTryMonad, A](a: A) extends UniWrapper[F,A] {

      override def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
        f(Success(a))

      override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
        f(Success(a))

      override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
        Cons[F,A](Success(a), Susp(() => other))

      override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
        summon[CpsTryMonad[F]].pure(Some((Success(a), Zero[F,A]())))

  }




  case class Cons[F[_]:CpsTryMonad,A](head: Try[A], tail: UniWrapper[F,A]) extends UniWrapper[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
          applyFlatMapTryToHead(f).mplus(tail.flatMapTry(f))

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
          applyFlatMapTryToHead(f).mplus(tail.flatMapTry(f))

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
          MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
          summon[CpsTryMonad[F]].pure(Some((head, tail)))

    private final def applyFlatMapTryToHead[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
          try
            f(head)
          catch
            case NonFatal(ex) =>
              WaitF[F,B](summon[CpsTryMonad[F]].error(ex))

  }


  case class Susp[F[_]:CpsTryMonad,A](susp: () => UniWrapper[F,A]) extends UniWrapper[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      // TODO: use type-aligned queue or trampolined function
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapper[F,?]]))

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      val next = summon[CpsTryMonad[F]].map(susp().flatMapTry(f).fsplit){
        case None => Zero[F,B]()
        case Some((head,tail)) => Cons[F,B](head,tail)
      }
      WaitF(next)

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
      susp().fsplit



  }


  case class FlatMapQueue[F[_]:CpsTryMonad,A,R](start: UniWrapper[F,A], queue: Queue[Try[?] => UniWrapper[F,?]]) extends UniWrapper[F,R] {

    override def flatMapTry[B](f: Try[R] => UniWrapper[F, B]): UniWrapper[F, B] =
      FlatMapQueue[F,A,B](start, queue.enqueue(f.asInstanceOf[Try[?] => UniWrapper[F,?]]))

    override def applyFlatMapTry[B](f: Try[R] => UniWrapper[F, B]): UniWrapper[F, B] =
      applyFlatMaps().applyFlatMapTry(f)

    override def mplus(other: => UniWrapper[F, R]): UniWrapper[F, R] =
      MPlusQueue[F,R](Queue(this,Susp(()=>other)))


    override def fsplit: F[Option[(Try[R], UniWrapper[F, R])]] =
      summon[CpsTryMonad[F]].flatMap(start.fsplit) {
        case None => summon[CpsTryMonad[F]].pure(None)
        case Some((startHead,startTail)) =>
          val startHeadUniWrapper = FlatMapQueue[F,A,R](fromTry(startHead), queue).applyFlatMaps()
          summon[CpsTryMonad[F]].flatMap(startHeadUniWrapper.fsplit) {
            case None => FlatMapQueue(startTail, queue).fsplit
            case Some((startHeadApplyHead, startHeadApplyTail)) =>
              val next = Susp(() => startHeadApplyTail mplus FlatMapQueue(startTail, queue))
              summon[CpsTryMonad[F]].pure(Some((startHeadApplyHead, next)))
          }
      }


    def applyFlatMaps(): UniWrapper[F,R] =
      val s0: UniWrapper[F,?] = start.asInstanceOf[UniWrapper[F,?]]
      val r = queue.foldLeft(s0){ (s,e) =>
        s.applyFlatMapTry(e.asInstanceOf[Try[s._R] => UniWrapper[F,Any]])
      }
      r.asInstanceOf[UniWrapper[F,R]]

  }



  case class WaitF[F[_]:CpsTryMonad,A](fa: F[UniWrapper[F,A]]) extends UniWrapper[F,A] {

    override def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      WaitF(
        summon[CpsTryMonad[F]].map(fa)(_.flatMapTry(f))
      )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      WaitF(
        summon[CpsTryMonad[F]].map(fa)(_.applyFlatMapTry(f))
      )

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))


    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
      summon[CpsTryMonad[F]].flatMap(fa)(_.fsplit)


  }


  case class MPlusQueue[F[_]:CpsTryMonad,A](queue: Queue[UniWrapper[F,A]]) extends UniWrapper[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
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
    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
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


    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      MPlusQueue[F,A](queue.enqueue(other))

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
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

  case class LVNew[F[_]:CpsTryMonad,A](lv: LogicalVariable[A]) extends UniWrapper[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapper[F,?]]) )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      // lv can participate in the rerursie expression
      LTerm[F,A](lv,LongMap.empty).applyFlatMapTry(f)

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class LVBind[F[_]:CpsTryMonad,A](lv: LogicalVariable[A], v: UniWrapper[F,A]) extends UniWrapper[F,A] {

    def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
      FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapper[F,?]]) )

    override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] = {
      toLTerm().applyFlatMapTry(f)
    }

    override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
      MPlusQueue[F,A](Queue(this,Susp(()=>other)))

    override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] =
      toLTerm().fsplit

    def toLTerm(): LTerm[F,A] =
      LTerm[F,A](lv,LongMap(lv.id -> LVarBingingRecord(lv,v)))

  }



  case class LVarBingingRecord[F[_],T](lv: LogicalVariable[T], v: UniWrapper[F,T]) {
    type Tp = T
  }

  case class SplittedLVarBingingRecord[F[_],T](lv: LogicalVariable[T],
                                                currentValue: Option[Try[T]],
                                                next: UniWrapper[F,T])


  case class LTerm[F[_]:CpsTryMonad,A](t: TypedLogicalTerm[A], bindings: LongMap[LVarBingingRecord[F,?]]) extends UniWrapper[F,A] {

      def flatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
        FlatMapQueue(this, Queue(f.asInstanceOf[Try[?] => UniWrapper[F,?]]) )

      override def applyFlatMapTry[B](f: Try[A] => UniWrapper[F, B]): UniWrapper[F, B] =
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

      override def mplus(other: => UniWrapper[F, A]): UniWrapper[F, A] =
          MPlusQueue[F,A](Queue(this,Susp(()=>other)))

      override def fsplit: F[Option[(Try[A], UniWrapper[F, A])]] = reify[F]{
          val optNext = reflect(fsplitInBinding(t, bindings))
          optNext match
            case None => None
            case Some((head, nextBindings)) =>
              head match
                case Success(t) =>
                  Some((Success(t), buildNextBindings(nextBindings.values.toIndexedSeq)))
                case Failure(ex) =>
                  Some((Failure(ex), buildNextBindings(nextBindings.values.toIndexedSeq)))
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
              case l: LogicalVariable[t] =>
                reify[F] {
                  val (optTryA, nextBinding) = reflect(getVarBinding(l.id, currentBinding))
                  val optTerm: Option[Try[LogicalTerm]] = optTryA.map(_.map(a =>
                    LogicalConstant[t](a.asInstanceOf[t])(using l.symbol)
                  ))
                  (optTerm, nextBinding)
                }
              case l: LogicalConstant[t] =>
                summon[CpsTryMonad[F]].pure(Some(Success(l:LogicalTerm)), currentBinding)
              case l: LogicalFunctionalTerm[t] =>
                val s0: (IndexedSeq[LogicalTerm], LongMap[SplittedLVarBingingRecord[F, ?]]) = (IndexedSeq.empty, currentBinding)
                reify[F] {
                  var done = false
                  var retval: Option[Try[LogicalTerm]] = None
                  var i = 0
                  var s = s0
                  while(!done && i<l.args.size) {
                    val (args, currentBinding) = s0
                    val (optTryArg, nextBinding) = reflect(fetchTerm(l.args(i), currentBinding))
                    optTryArg match
                      case None =>
                        retval = None
                        done = true
                      case Some(tryArg) =>
                        tryArg match
                          case Failure(ex) =>
                             retval = Some(Failure(ex))
                             done = true                         
                          case Success(arg) =>
                             s = (args :+ arg) -> nextBinding
                    i = i+1        
                  }
                  val (nextArgs, nextBindings) = s
                  if (!done) {
                    val lt: LogicalTerm = LogicalFunctionalTerm(nextArgs)(using l.symbol)
                    (Some(Success(lt)), nextBindings)
                  } else {
                    (retval, nextBindings)
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
          optTryA.map((_,bindings))
        }


      }


      private def buildNextBindings(nextBindings: Seq[SplittedLVarBingingRecord[F,?]]): UniWrapper[F,A] = {
        // here is logical term with already evaluated variables is emitted,  now we need to emit
        // rest of possible variants.
        // Generally, the order of emitting can be defined by configurable strategy.
        // For now, we follow BFS strategy.

        // let <n> is number of variables in term, represent 2^n as a set of bits, where 1 means that
        // we should emit next variant of variable, 0 - we should emit current value.
        // then, first variant is 0..0  (emitted before call of nextBinfdings), last is 1..1



        def buildVariant(bits:Int): UniWrapper[F,A] = {
          val argsBuilder = IndexedSeq.newBuilder[LogicalTerm]
          var newBindings = LongMap[LVarBingingRecord[F, ?]]()
          var i = 0
          var currentBit = 1
          var done = false
          var empty = false
          var retval: UniWrapper[F, A] = UniWrapper.Zero[F, A]()
          while (i < nextBindings.size && !done && !empty) {
            val lvr = nextBindings(i)
            val lv = lvr.lv
            val bit = (bits & currentBit) != 0
            if (bit) {
              newBindings = newBindings.updated(i, LVarBingingRecord(lv, lvr.next))
              argsBuilder.addOne(lv)
            } else {
              lvr.currentValue match
                case None =>
                  empty = true
                  done = true
                case Some(tryA) =>
                  tryA match
                    case Failure(ex) =>
                      //we should have only one Failure, so
                      // check - if this is a minimal variant, then we should return it
                      //  otherwise - Zero to omitt duplicates
                      if (bits == currentBit - 1) {
                        retval = UniWrapper.WaitF[F, A](summon[CpsTryMonad[F]].error(ex))
                      } else {
                        retval = UniWrapper.Zero[F, A]()
                      }
                      done = true
                    case Success(a) =>
                      argsBuilder.addOne(LogicalConstant[lv.Type](a)(using lv.symbol))
            }
            i = i + 1
            currentBit = currentBit << 1
          }
          if (done) {
            retval
          } else if (empty) {
            UniWrapper.Zero[F, A]()
          } else {
            val args = argsBuilder.result()
            val term = LogicalFunctionalTerm(args)(using t.symbol)
            val nextTerm = LTerm[F, A](term, newBindings)
            if (bits + 1 == (1 << nextBindings.size))
              nextTerm
            else
              nextTerm.mplus(buildVariant(bits + 1))
          }

        }

        buildVariant(1)

      }


  }

  def fromTry[F[_]:CpsTryMonad,A](t: Try[A]): UniWrapper[F,A] =
    t match
      case Success(a) => Pure[F,A](a)
      case Failure(ex) => WaitF(summon[CpsTryMonad[F]].error(ex))

  def error[F[_]:CpsTryMonad,A](msg: String): UniWrapper[F,A] =
    WaitF(summon[CpsTryMonad[F]].error(new RuntimeException(msg)))

  class UniWrapperEnvironment[ F[_]:CpsTryMonad] extends UnificationEnvironment[[T]=>>UniWrapper[F,T]] {

    thisUnificationEnvironment =>

    class LContext extends UnificationEnvironmentContext[[T]=>>UniWrapper[F,T]] {
      override def monad: UnificationEnvironment[[T] =>> UniWrapper[F, T]] = thisUnificationEnvironment
    }

    override type Context = LContext

    override type Observer[X] = F[X]

    def pure[A](a:A): UniWrapper[F,A] =
      Pure[F,A](a)

    def map[A,B](fa: UniWrapper[F,A])(f: A=>B): UniWrapper[F,B] = ???

    def flatMap[A,B](fa: UniWrapper[F,A])(f: A=>UniWrapper[F,B]): UniWrapper[F,B] = ???

    def flatMapTry[A,B](fa: UniWrapper[F,A])(f: Try[A]=>UniWrapper[F,B]): UniWrapper[F,B] =
      fa.flatMapTry(f)

    override def error[A](e: Throwable): UniWrapper[F, A] = ???

    override def mzero[A]: UniWrapper[F, A] = ???

    override def mplus[A](x: UniWrapper[F, A], y: =>UniWrapper[F, A]): UniWrapper[F, A] = ???

    override def apply[T](op: LContext => UniWrapper[F, T]): UniWrapper[F, T] = {
      op(new LContext)
    }

    override def msplit[A](c: UniWrapper[F, A]): UniWrapper[F, Option[(Try[A], UniWrapper[F, A])]] = ???

    override def observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def mObserveOne[A](ma: UniWrapper[F, A]): F[Option[A]] = ???

    override def mFoldLeft[A, B](ma: UniWrapper[F, A], zero: F[B])(op: (F[B], F[A]) => F[B]): F[B] = ???

    override def mFoldLeftN[A, B](ma: UniWrapper[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = ???

    //TODO: change to bind with M[A] instead of term
    override def bindTerm[A:Unifiable](lv: LogicalVariable[A], t: TypedLogicalTerm[A]): UniWrapper[F, A] = ???

    override def bind[A:Unifiable](lv: LogicalVariable[A], v: UniWrapper[F, A]): UniWrapper[F, A] = ???

    override def fromTerm[A: Unifiable](t: TypedLogicalTerm[A]): UniWrapper[F, A] = ???

    override def toTerm[A: Unifiable](ma: UniWrapper[F, A]): UniWrapper[F, TypedLogicalTerm[A]] = ???


    def cons[T](head: Try[T], tail: UniWrapper[F,T]): UniWrapper[F,T] =
      Cons[F,T](head,tail)

    def susp[T](susp: () => UniWrapper[F,T]): UniWrapper[F,T] =
      Susp[F,T](susp)

    def waitF[T](fa: F[UniWrapper[F,T]]): UniWrapper[F,T] =
      WaitF[F,T](fa)

    def mplusQueue[T](queue: Queue[UniWrapper[F,T]]): UniWrapper[F,T] =
      MPlusQueue[F,T](queue)

    def lvBind[T](lv: LogicalVariable[T], v: UniWrapper[F,T]): UniWrapper[F,T] =
      LVBind[F,T](lv,v)

    def lTerm[T](t: TypedLogicalTerm[T], bindings: LongMap[LVarBingingRecord[F,?]]): UniWrapper[F,T] =
      LTerm[F,T](t,bindings)

  }


}

