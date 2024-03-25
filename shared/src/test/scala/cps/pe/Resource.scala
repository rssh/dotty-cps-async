package cps.pe


import scala.util.*
import cps.*


sealed trait Resource[F[_],A]  {

  def run():F[A] = ???

}

object Resource {

  case class Allocate[F[_],A](allocFun: () => F[(A, () => F[Unit])]) extends Resource[F,A]
  case class BindTry[F[_], A, B](source: Resource[F, A], next: Try[A] => Resource[F, B]) extends Resource[F, B]
  case class Pure[F[_],A](a:A) extends Resource[F,A]
  case class Error[F[_],A](e: Throwable) extends Resource[F,A]
  case class Eval[F[_],A](fa:F[A]) extends Resource[F,A]

  class ResourceMonad[F[_]](mf: CpsTryMonad[F]) extends CpsTryMonad[[X]=>>Resource[F,X]]
                                                           with CpsTryMonadInstanceContext[[X]=>>Resource[F,X]] {
    def pure[A](a:A): Resource[F,A] = Eval(mf.pure(a))
    def map[A,B](fa: Resource[F,A])(f: A=>B): Resource[F,B] =
      flatMap(fa)(a => pure(f(a)))
    def flatMap[A,B](fa: Resource[F,A])(f: A=>Resource[F,B]): Resource[F,B] =
      BindTry(fa,{
        case Success(a) => f(a)
        case Failure(ex) => Eval(mf.error(ex))
      })
    def error[A](e: Throwable): Resource[F,A] = Error(e)
    def flatMapTry[A,B](fa: Resource[F,A])(f: Try[A]=>Resource[F,B]): Resource[F,B] =
      BindTry(fa,f)
  }


  def run[F[_],A](v:Resource[F,A])(using mf: CpsTryMonad[F]):F[A] = {

    sealed trait NextStack
    case object Empty extends NextStack
    case class Frame[AA, BB](next: Try[AA] => Resource[F, BB], tail: NextStack) extends NextStack {
      def applyTop(t: Try[?]): Resource[F, BB] = next(t.asInstanceOf[Try[AA]])
    }


    def loop(v: Resource[F, ?], nextStack: NextStack, finStack: List[() => F[Unit]])(using mf: CpsTryMonad[F]): F[A] = {
      v match
        case Allocate(allocFun) =>
          mf.flatMapTry(allocFun()) {
            case Success((a, fin)) =>
              loop(Pure(a), nextStack, fin :: finStack)
            case Failure(ex) =>
              loop(Eval(mf.error(ex)), nextStack, finStack)
          }
        case BindTry(source, next) =>
          loop(source, Frame(next, nextStack), finStack)
        case Eval(fa) =>
          mf.flatMapTry(fa) {
            case Success(a) =>
              loop(Pure(a), nextStack, finStack)
            case Failure(ex) =>
              loop(Error(ex), nextStack, finStack)
          }
        case Pure(a) =>
          nextStack match
            case Empty =>
              mf.flatMap(releaseList(finStack))(_ => mf.pure(a.asInstanceOf[A]))
            case frame@Frame(next, nextStack) =>
              loop(frame.applyTop(Success(a)), nextStack, finStack)
        case Error(ex) =>
          nextStack match
            case Empty =>
              mf.flatMap(releaseList(finStack))(_ => mf.error(ex))
            case Frame(next, nextStack) =>
              loop(next(Failure(ex)), nextStack, finStack)
    }

    def releaseList(value: List[() => F[Unit]])(using mf: CpsTryMonad[F]): F[Unit] = {
      value match
        case Nil => mf.pure(())
        case h::t => mf.flatMapTry(h()) {
          case Success(_) => releaseList(t)
          case Failure(ex) => mf.flatMap(releaseList(t))(_ => mf.error(ex))
        }
    }

    loop(v, Empty, Nil)
  }

  given resourceMonad[F[_]](using m:CpsTryMonad[F]):ResourceMonad[F] =
    new ResourceMonad[F](m)

  given toF[F[_]](using m:CpsTryMonad[F]): CpsMonadConversion[F,[X]=>>Resource[F,X]] = {
    new CpsMonadConversion[F,[X]=>>Resource[F,X]] {
      def apply[T](ft: F[T]): Resource[F,T] = Eval(ft)
    }
  }

}

