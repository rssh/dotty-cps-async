package logic

import scala.util.*

import cps.*

trait CpsLogicMonad[M[_]] extends CpsTryMonad[M] {

  override type Context <: CpsLogicMonadContext[M]

  def mzero[A]: M[A]

  def mplus[A](a: M[A], b: => M[A]): M[A]

  def msplit[A](c: M[A]): M[Option[(Try[A], M[A])]]

  def interleave[A](a: M[A], b: M[A]): M[A] = {
    flatMap(msplit(a)) { sa =>
      sa match
        case None => b
        case Some((ta, sa1)) =>
          mplus(fromTry(ta), interleave(b, sa1))
    }
  }

  /**
   *   >>- in haskell LogicT
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

  def once[A](a: M[A]): M[A] = {
    flatMap(msplit(a)) { sc =>
      sc match
        case None => mzero
        case Some((ta, sa)) =>
          fromTry(ta)
    }
  }

  type Observer[A]

  def observerCpsMonad: CpsTryMonad[Observer]

  def mObserveOne[A](ma:M[A]): Observer[Option[A]]

  def mObserveN[A](ma: M[A], n: Int): Observer[Seq[A]] =
    mFoldLeftN(ma, observerCpsMonad.pure(IndexedSeq.empty[A]), n) { (observer, fa) =>
      observerCpsMonad.flatMap(observer)(seq => observerCpsMonad.map(fa)(a => seq :+ a))
    }
  
  def mObserveAll[A](ma: M[A]): Observer[Seq[A]] =
    mFoldLeft(ma, observerCpsMonad.pure(IndexedSeq.empty[A])) { (observer, fa) =>
      observerCpsMonad.flatMap(observer)(seq => observerCpsMonad.map(fa)(a => seq :+ a))
    }

  def mFoldLeft[A,B](ma:M[A], zero:Observer[B])(op: (Observer[B],Observer[A])=>Observer[B]): Observer[B]

  def mFoldLeftN[A,B](ma: M[A], zero: Observer[B], n: Int)(op: (Observer[B],Observer[A])=>Observer[B]): Observer[B]

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

def all[M[_],A](collection: IterableOnce[A])(using m:CpsLogicMonad[M]): M[A] =
   def allIt(it: Iterator[A]): M[A] =
      if (it.hasNext) {
        m.mplus(m.pure(it.next()), allIt(it))
      } else {
        m.mzero
      }
   allIt(collection.iterator)





extension [M[_],A](ma: M[A])(using m:CpsLogicMonad[M])

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

  def observeN(n: Int): m.Observer[Seq[A]] =
    m.mObserveN(ma,n)

  def observeOne: m.Observer[Option[A]] =
    m.mObserveOne(ma)

  def observeAll: m.Observer[Seq[A]] =
    m.mObserveAll(ma)

  def |+|(mb: =>M[A]): M[A] =
    m.mplus(ma,mb)
    
  def ||(mb: =>M[A]): M[A] =
    m.mplus(ma,mb)
    
  def |(mb: =>M[A]): M[A] =
    m.interleave(ma,mb)  


transparent inline def guard[M[_]:CpsLogicMonad](p: =>Boolean)(using mc:CpsLogicMonadContext[M]): Unit =
  reflect{
    if (p) mc.monad.pure(()) else mc.monad.mzero
  }
