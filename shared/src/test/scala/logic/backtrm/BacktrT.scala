package logic.backtrm

//Term implementation of backtracking monad transformer
// from Deriving Backtracking Monad Transformers by Ralf Hinze
// https://dl.acm.org/doi/10.1145/351240.351258
//
// changed to support flatMapTry

import cps.*
import cps.syntax.*
import scala.util.*
import scala.util.control.NonFatal



sealed trait BacktrT[M[_],A] {

  def map[B](f: A=>B): BacktrT[M,B]
  def flatMap[B](f: A=>BacktrT[M,B]): BacktrT[M,B]
  def flatMapTry[B](f: Try[A]=>BacktrT[M,B]): BacktrT[M,B]
  def append(next: =>BacktrT[M,A]): BacktrT[M,A]


}

object BacktrT{

  case class Zero[M[_],A]() extends BacktrT[M,A] {
    override def map[B](f: A=>B): Zero[M,B] = Zero()
    override def flatMap[B](f: A=>BacktrT[M,B]): Zero[M,B] = Zero()
    override def flatMapTry[B](f: Try[A]=>BacktrT[M,B]): Zero[M,B] = Zero()
    override def append(next: =>BacktrT[M, A]): BacktrT[M, A] = next

  }

  case class Cons[M[_]:CpsTryMonad,A](h: A, t: () => BacktrT[M,A]) extends BacktrT[M,A] {

    def map[B](f: A=>B): Cons[M,B] = Cons(f(h), () => t().map(f))

    def flatMap[B](f: A=>BacktrT[M,B]): BacktrT[M,B] =
      f(h).append(t().flatMap(f))

    override def append(next: =>BacktrT[M, A]): BacktrT[M, A] =
      Cons(h, () => t().append(next))

    override def flatMapTry[B](f: Try[A]=>BacktrT[M,B]): BacktrT[M,B] =
      try {
        f(Success(h)).append(t().flatMapTry(f))
      } catch {
        case NonFatal(ex) =>
          PromoteBind(summon[CpsTryMonad[M]].error(ex), {
            case Success(_) => Success(Zero())  // impossible
            case Failure(ex) => Failure(ex)
          })
      }

  }


  case class PromoteBind[M[_],A,B](ma: M[A], transform: Try[A]=>Try[BacktrT[M,B]]) extends BacktrT[M,B] {

    def map[C](f: B=>C): PromoteBind[M,A,C] = PromoteBind(ma,
      { case Success(a) => transform(Success(a)).map(_.map(f))
        case Failure(ex) => Failure(ex)
      }
    )

    def flatMap[C](g: B=>BacktrT[M,C]): BacktrT[M,C] = PromoteBind(ma, {
      case Success(a) => transform(Success(a)).map(_.flatMap(g))
      case Failure(ex) => Failure(ex)
    })

    override def append(next: => BacktrT[M, B]): BacktrT[M, B] =
      PromoteBind(ma, {
        case Success(a) => transform(Success(a)).map(_.append(next))
        case Failure(ex) => Failure(ex)
      })


    override def flatMapTry[C](g: Try[B]=>BacktrT[M,C]): BacktrT[M,C] =
      PromoteBind(
        ma, { x =>
          transform(x) match
            case Success(bm) => Success(bm.flatMapTry(g))
            case Failure(ex) => Success(g(Failure(ex)))
        }
      )


  }
  
  

  class CpsBacktrTryMonad[M[_]:CpsTryMonad] extends CpsTryMonad[[X]=>>BacktrT[M,X]] with CpsTryMonadInstanceContext[[X]=>>BacktrT[M,X]] {

    override def pure[A](a: A): BacktrT[M, A] = Cons(a,()=>Zero())

    override def map[A,B](fa: BacktrT[M,A])(f: A=>B): BacktrT[M,B] =
      fa.map(f)

    override def flatMap[A,B](fa: BacktrT[M,A])(f: A=>BacktrT[M,B]): BacktrT[M,B] =
       fa.flatMap(f)

    override def error[A](e: Throwable): BacktrT[M, A] =
       PromoteBind(summon[CpsTryMonad[M]].error(e), {
          case Success(_) => Failure(e) // impossible
          case Failure(ex) => Failure(ex)
       })

    override def flatMapTry[A,B](fa: BacktrT[M,A])(f: Try[A]=> BacktrT[M,B]): BacktrT[M,B] =
        fa.flatMapTry(f)

   

  }

}