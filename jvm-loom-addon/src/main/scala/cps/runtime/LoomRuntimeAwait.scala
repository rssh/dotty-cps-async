package cps.runtime

import cps.*

trait LoomRuntimeAwait[F[_]] extends CpsRuntimeAwait[F] {

  def await[A](fa: F[A])(ctx: CpsTryMonadContext[F]): A

}
