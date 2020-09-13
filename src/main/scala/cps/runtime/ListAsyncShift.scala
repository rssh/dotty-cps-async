package cps.runtime

import cps._
import scala.collection._


class ListAsyncShift[A] extends IterableOpsAsyncShift[A,List, List[A]] with AsyncShift[List[A]] {

  override def map[F[_], B](c: List[A], monad: CpsMonad[F])(f: A=> F[B]):F[List[B]] =
    c match
      case Nil => monad.pure(Nil)
      case head::tail => val hbf = f(head)
                         val tbf = map(tail,monad)(f)
                         monad.flatMap(hbf)(hb => 
                               monad.map(tbf)(tb => hb::tb))


}

