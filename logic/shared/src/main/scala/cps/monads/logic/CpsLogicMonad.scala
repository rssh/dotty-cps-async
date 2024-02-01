package cps.monads.logic

import cps.*

import scala.annotation.tailrec
import scala.util.*

/**
 * Typeclass for monad with backtracking logic operations.
 * We can interpret it as a monad with non-deterministic choice or as a
 * potentialy infinite stream of values corresponding to possible choices.
 *
 * The default implementation is LogicStream (for no need in async operations)
 * or LogicStreamT[F] (for need in async operations)
 *
 *
 * @tparam M
 */
trait CpsLogicMonad[M[_]] extends CpsTryMonad[M] {

  override type Context <: CpsLogicMonadContext[M]

  /**
   * Monad, which can be used to observe values of computation
   * and can be used in map/flatMap/... operations.
   * Usefull for cases, where we should mix logic and async operations.
   * If implementation is LogicStream[F[_],A], that Observer is F[_].
   */
  type Observer[A]

  /**
   * instance of observer cps monad.
   */
  val observerCpsMonad: CpsTryMonad[Observer]


  /**
   *  mzero in haskell LogicT
   *  Synonym for 'empty'.
   */
  def mzero[A]: M[A]

  /**
   *  empty solution set.
   *  (same as 'mzero')
   */
  def empty[A]: M[A] = mzero

  /**
   * mplus in haskell LogicT
   *  Synonym for 'orElse'.
   */
  def mplus[A](a: M[A], b: => M[A]): M[A]

  /**
   * Create a computation, which explore <code> a <code> and then <code> b </code>
   * end do not start evaluation of <code> b </code> until all values or <code> a </code> are explored.
   *
   * Synonym for MonadPlus 'mplus'  or Alternative '<|>' in haskell.
   */
  def seqOr[A](a: M[A], b: => M[A]): M[A] = mplus(a,b)

  /**
   * Split computation into part, which return as first value and rest of computation
   * and return this as logic stream.
   * @param c
   * @tparam A
   * @return
   */
  def msplit[A](c: M[A]): M[Option[(Try[A], M[A])]]

  /**
   * Split computation into part, which return as first value and rest of computation
   * and return this as observer.
   * @param c
   * @tparam A
   * @return
   */
  def fsplit[A](c: M[A]): Observer[Option[(Try[A], M[A])]]

  def unsplit[A](ta: Try[A], m:M[A]): M[A] =
    mplus(fromTry(ta), m)

  /**
   * Flatten observer into computation.
   * @param fma - observer to flatten
   * @return - computation, which adopt observer wrapper
   */
  def flattenObserver[A](fma: Observer[M[A]]): M[A]

  /**
   *  Can be viewed as `fair Or` -- values from both computations are interleaved
   */
  def interleave[A](a: M[A], b: M[A]): M[A] = {
    flatMap(msplit(a)) { sa =>
      sa match
        case None => b
        case Some((ta, sa1)) =>
          mplus(fromTry(ta), interleave(b, sa1))
    }
  }

  /**
   * Can be viewed as `fair And` -- flatMap results are mixed over the all choices in <code> ma </code>
   *
   * >>- in haskell LogicT
   */
  def fairFlatMap[A, B](ma: M[A], mb: A => M[B]): M[B] = {
    flatMap(msplit(ma)) { sa =>
      sa match
        case None => mzero
        case Some((ta, sa1)) =>
          ta match
            case Success(a) =>
              interleave(mb(a), fairFlatMap(sa1, mb))
            case Failure(ex) =>
              error(ex)
    }
  }

  /**
   * ifte -- if/then/else which works not on boolean condition, but on
   *    existence of values in computation.
   * @param a - computation to check
   * @param thenp - what to do after <code> a </code>
   * @param elsep - what to do if <code> a </code> is empty
   */
  def ifte[A, B](a: M[A], thenp: A => M[B], elsep: M[B]): M[B] = {
    flatMap(msplit(a)) { sc =>
      sc match
        case None => elsep
        case Some((ta, sa)) =>
          ta match
            case Success(a) =>
              mplus(thenp(a), flatMap(sa)(thenp))
            case Failure(ex) =>
              error(ex)
    }
  }

  /**
   * get the first value of computation, discarding all other.
   * head of the list, or soft cut in prolog (scoped inside current computation).
   * @param a
   * @tparam A
   * @return
   */
  def once[A](a: M[A]): M[A] = {
    flatMap(msplit(a)) { sc =>
      sc match
        case None => mzero
        case Some((ta, sa)) =>
          fromTry(ta)
    }
  }


  def mObserveOne[A](ma:M[A]): Observer[Option[A]]

  def mObserveN[A](ma: M[A], n: Int): Observer[IndexedSeq[A]] =
    mFoldLeftWhile(ma,IndexedSeq.empty[A], (seq:IndexedSeq[A]) => seq.size < n) { (seq,a) =>
      seq :+ a
    }

  def mFoldLeftWhileM[A,B](ma:M[A], zero: Observer[B], p: B=>Boolean)(op: (Observer[B],Observer[A])=>Observer[B]): Observer[B]

  def mFoldLeftWhile[A,B](ma:M[A], zero: B, p: B=>Boolean)(op: (B,A)=>B): Observer[B] = {
    mFoldLeftWhileM(ma, observerCpsMonad.pure(zero), p) { (bObs,aObs) =>
      observerCpsMonad.flatMap(bObs) { b =>
        observerCpsMonad.flatMap(aObs) { a =>
          observerCpsMonad.pure(op(b,a))
        }
      }
    }
  }

  def fromCollection[A](collect: IterableOnce[A]): M[A] = {
    def fromIt(it: Iterator[A]): M[A] =
      if (it.hasNext) {
        mplus(pure(it.next()), fromIt(it))
      } else {
        mzero
      }
    fromIt(collect.iterator)
  }

}



object CpsLogicMonad {

  type Aux[M[+_],F[_]] = CpsLogicMonad[M] {
    type Observer[T] = F[T]

  }

}


trait CpsLogicMonadContext[M[_]] extends CpsTryMonadContext[M] {

  override def monad: CpsLogicMonad[M]

}

class CpsLogicMonadInstanceContextBody[M[_]](m:CpsLogicMonad[M]) extends CpsLogicMonadContext[M] {
  override def monad: CpsLogicMonad[M] = m
}


trait CpsLogicMonadInstanceContext[M[_]] extends CpsLogicMonad[M]  {

  override type Context = CpsLogicMonadInstanceContextBody[M]

  override def apply[T](op: CpsLogicMonadInstanceContextBody[M] => M[T]): M[T] = {
    op(new CpsLogicMonadInstanceContextBody[M](this))
  }

}

/**
 * Transform collection into logical stream, which include all elements.
 * @param collection - collection to transform
 * @param m - logical monad to use.
 */
def all[M[_],A](collection: IterableOnce[A])(using m:CpsLogicMonad[M]): M[A] =
   def allIt(it: Iterator[A]): M[A] =
      if (it.hasNext) {
        m.mplus(m.pure(it.next()), allIt(it))
      } else {
        m.mzero
      }
   allIt(collection.iterator)


/**
 * CpsLogicMonad extension methods.
 */
extension [M[_],A](ma: M[A])(using m:CpsLogicMonad[M])

  /**
   * filter values, which satisfy predicate.
   */
  def filter(p: A => Boolean): M[A] =
    m.flatMap(m.msplit(ma)) { sc =>
      sc match
        case None => m.mzero
        case Some((ta, sa)) =>
          ta match
            case Success(a) =>
              if (p(a)) {
                m.mplus(m.pure(a), sa.filter(p))
              } else {
                sa.filter(p)
              }
            case Failure(ex) =>
              m.error(ex)
    }

  /**
   * get first N values of computation, discarding all other.
   * @param n - how many values to get
   * @return - sequence of values in observer monad
   * @see cps.monads.logic.CpsLogicMonad.mObserveN        
   */
  def observeN(n: Int): m.Observer[IndexedSeq[A]] =
    m.mObserveN(ma,n)

  /**
   * get first value of computation.
   * @see cps.monads.logic.CpsLogicMonad.mObserveOne
   */
  def observeOne: m.Observer[Option[A]] =
    m.mObserveOne(ma)

  // avoid publish methid which can cause infinite loop
  //def observeAll: m.Observer[Seq[A]] =
  //  m.mObserveAll(ma)

  /**
   * Synonym for 'mplus'.
   * @param mb - computation to add
   * @return - stream, which contains values from <code> ma </code> and when <code> ma </code> is exhaused - <code> mb </code>
   * @see cps.monads.logic.CpsLogicMonad.mplus        
   */
  def |+|(mb: =>M[A]): M[A] =
    m.mplus(ma,mb)

  /**
   * Synonym for 'mplus'.
   * @see cps.monads.logic.CpsLogicMonad.mplus 
   */  
  def ||(mb: =>M[A]): M[A] =
    m.mplus(ma,mb)

  /**
   * interleave current computation with <code> mb </code>
   * @param mb computation to interleave.
   * @return - stream, which contains values from <code> ma </code> and <code> mb </code> in interleaved order.
   * @see cps.monads.logic.CpsLogicMonad.interleave        
   */
  def |(mb: =>M[A]): M[A] =
    m.interleave(ma,mb)

  /**
   * Synonym for 'fairFlatMap' or haskell >>-
   * @param f
   * @tparam B
   * @return - stream, which contains values from <code> ma </code> and <code> f </code> applied to each value of <code> ma </code>
   *         in interleaved order.
   * @see cps.monads.logic.CpsLogicMonad.fairFlatMap         
   */
  def &>>[B](f: A=>M[B]): M[B] =
    m.fairFlatMap(ma,f)

  /**
   * Version of flatMap, which interleave all results of ma
   * (i.e. horizontal search instead of bfs).
    * @param f - function to apply to each value of <code> ma </code>
   *  @see cps.monads.logic.CpsLogicMonad.fairFlatMap        
   */  
  def fairFlatMap[B](f: A=>M[B]): M[B] =
    m.fairFlatMap(ma,f)
  
  /**
   * retrieve only first value of computation.
   * @return - stream, which contains only first value of <code> ma </code>
   * @see cps.monads.logic.CpsLogicMonad.once        
   */
  def once: M[A] =
    m.once(ma)

  /**
   * If <code> ma </code> is note empty, then run <code> thenp </code> on it else <code> elsep </code>,
   * @param thenp
   * @param elsep
   * @tparam B
   * @return
   * @see cps.monads.logic.CpsLogicMonad.ifte
   */
  def ifThenElse[B](thenp: A => M[B], elsep: M[B]): M[B] =
    m.ifte(ma, thenp, elsep)
  
  /**
   * Run <code> thenp </code> if <code> ma </code> is empty.
   * @param thenp
   * @return
   * @see cps.monads.logic.CpsLogicMonad.otherwise
   */
  def otherwise(thenp: =>M[A]): M[A] =
    m.ifte(ma, (a:A) => m.pure(a), thenp) 
    
    


/**
 * Should be used inside of reify block over CpsLogicMonad.
 * The next sequent code will be executed only if <code> p </code> is true,
 * otherwise the computation of the current value of logical stream will be terminated.
 * @param p - predicate to check
 * @param mc - monad context
 * @tparam M
 */
transparent inline def guard[M[_]](p: =>Boolean)(using mc:CpsLogicMonadContext[M]): Unit =
  reflect{
    if (p) mc.monad.pure(()) else mc.monad.mzero
  }

/**
 * Should be used inside of reify block over CpsLogicMonad.
 * The next sequent code will be executed for all elements of <code> collection </code>
 * @param collection - collection to iterate over
 * @param mc - monad context
 * @tparam M
 */
transparent inline def choicesFrom[M[_],A](collection: IterableOnce[A])(using mc:CpsLogicMonadContext[M]): A =
  reflect{
    mc.monad.fromCollection(collection)
  }

/**
 * Should be used inside of reify block over CpsLogicMonad.
 * The next sequent code will be executed for all <code> values </code>
 * @param values - values to iterate over
 * @param mc - monad context
 * @tparam M
 */
transparent inline def choices[M[_],A](values: A*)(using mc:CpsLogicMonadContext[M]): A =
  reflect{
    mc.monad.fromCollection(values)
  }
