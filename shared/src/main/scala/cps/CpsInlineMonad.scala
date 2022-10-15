package cps

import scala.util.*
import scala.util.control.NonFatal

/**
 * Inline cps monad which can used for
 * (usefull for deep embedding of other languages into scala, sending data over network, etc..)   
 **/
trait CpsInlineMonad[F[_]] extends CpsContextCarrier[F] {

    inline def lazyPure[A](inline a:A): F[A]

    inline def pure[A](inline a:A): F[A]

    inline def map[A,B](inline fa:F[A])(f: A=>B):F[B]

    inline def flatMap[A,B](inline fa: F[A])(f: A=>F[B]):F[B]

}

object CpsInlineMonad {

    type Aux[F[_],C] = CpsInlineMonad[F] { type Context = C }

}


trait CpsInlineTryMonad[F[_]] extends CpsInlineMonad[F] {

    inline def error[A](inline e: Throwable): F[A]

    inline def mapTry[A,B](inline fa:F[A])(inline f: Try[A]=>B):F[B] =
        flatMapTry(fa)(x => pure(f(x)))

    inline def flatMapTry[A,B](inline fa: F[A])(inline f: Try[A]=>F[B]):F[B]


    inline def withAction[A](inline fa: F[A])(inline action: Unit): F[A] = {
        flatMapTry(fa){ x =>
             action
             x match
              case Success(v) => pure(v)
              case Failure(e) => error(e)
        }
    }

    inline def withAsyncAction[A](inline fa: F[A])(inline action: F[Unit]): F[A] = {
        flatMapTry(fa){ x =>
             flatMapTry(action){ u =>
                u match
                    case Success(_) =>
                        x match
                            case Success(xv) => pure(xv)
                            case Failure(xe) => error(xe)
                    case Failure(uex) =>
                        x match
                            case Success(xv) => error(uex)
                            case Failure(xe) =>
                                uex.addSuppressed(xe)
                                error(uex)
             }
        }
    }

      

    inline def tryImpure[A](inline fa: F[A]):F[A] = {
        try {
            fa
        }catch{
            case  NonFatal(ex) =>
                error[A](ex)
        }
    }

    inline def tryPure[A](inline a: A):F[A] = {
        try {
            pure(a)
        }catch{
            case NonFatal(ex) =>
                error[A](ex)
        }
    }
   


}

object CpsInlineTryMonad {

    type Aux[F[_],C] = CpsInlineTryMonad[F] { type Context = C }

}
