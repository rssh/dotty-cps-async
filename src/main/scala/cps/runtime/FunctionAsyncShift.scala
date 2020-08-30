package cps.runtime

import cps._

class Function1AsyncShift[T1,R] extends AsyncShift[Function1[T1,R]]:

   def andThen[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: (R) => F[A]): T1 => F[A] =
        f.andThen(g)

   def compose[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: A => F[T1]): A => F[R] =
        x => m.map(g(x))(f)

   
