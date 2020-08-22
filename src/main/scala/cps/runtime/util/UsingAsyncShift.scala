package cps.runtime.util

import scala.util._
import cps._

//TODO: tests
object UsingAsyncShift extends AsyncShift[Using.type]:

   def apply[F[_], R, A](o: Using.type, m:CpsTryMonad[F])(resource: () => F[R])(f: (R) => F[A])(implicit arg0: Using.Releasable[R]): F[Try[A]] = {
      m.flatMap(resource())(r => 
        m.restore(
          m.map(f(r))(x=>{ arg0.release(r);  Success(x) })
        )(e=>m.pure(Failure(e))))
   }

   def resource[F[_], R, A](o: Using.type, m:CpsTryMonad[F])(r: R)(f: (R) => F[A])(implicit arg0: Using.Releasable[R]): F[A] =
    m.withAction(f(r))(arg0.release(r))


