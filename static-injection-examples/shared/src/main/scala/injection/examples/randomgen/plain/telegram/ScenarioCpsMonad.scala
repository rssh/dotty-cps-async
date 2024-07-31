package injection.examples.randomgen.plain.telegram

import canoe.api.Scenario
import cps.{CpsTryMonad, CpsTryMonadInstanceContext}

import scala.util.{Failure, Success, Try}

class ScenarioCpsMonad[F[_]] extends CpsTryMonad[[X] =>> Scenario[F, X]] with CpsTryMonadInstanceContext[[X] =>> Scenario[F, X]] {
  override def pure[T](t: T): Scenario[F, T] = Scenario.pure[F](t)

  override def map[A, B](fa: Scenario[F, A])(f: A => B): Scenario[F, B] = fa.map(f)

  override def flatMap[A, B](fa: Scenario[F, A])(f: A => Scenario[F, B]): Scenario[F, B] = fa.flatMap(f)

  override def error[A](e: Throwable): Scenario[F, A] = Scenario.raiseError(e)

  override def flatMapTry[A, B](fa: Scenario[F, A])(f: Try[A] => Scenario[F, B]): Scenario[F, B] =
    fa.map(Success.apply)
      .handleErrorWith(failure => Scenario.pure(Failure(failure)))
      .flatMap(f)
}

given [F[_]]: ScenarioCpsMonad[F] = ScenarioCpsMonad[F]
