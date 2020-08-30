package cps.runtime

import cps._

// Nothing will work in current version (needs implement or inlining or shifted-lambda)
class Function1AsyncShift[T1,R] extends AsyncShift[Function1[T1,R]]:

   def qqq = 1

   //def andThen[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: (R) => F[A]): T1 => F[A] =
   //    f.andThen(g)

   // inline not aplied here for some reason.
   //inline def andThen[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: (R) => F[A]): F[T1 => A] =
   //       m.pure[T1=>A](a => await(g(f(a)))(using m))

   //def compose[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: A => F[T1]): A => F[R] =
   //     x => m.map(g(x))(f)

   
