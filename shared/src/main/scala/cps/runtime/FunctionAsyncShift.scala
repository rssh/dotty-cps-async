package cps.runtime

import cps._


// Nothing will work in current version (needs implement or inlining or shifted-lambda)
class Function1AsyncShift[T,R] extends AsyncShift[Function1[T,R]]:

   def qqq = 1

   // TODO: maybe add extra parameter or change name for shifted variant (?)
   //def andThen[F[_],A](f: (T)=>R, m: CpsMonad[F])(g: (R) => F[A]): T => F[A] =
   //    f.andThen(g)

   def andThen[F[_],A](f: T=>R, m: CpsMonad[F])(g: (R) => F[A]): Function1AndThenCallChainSubst[F,T,R,A] =
        Function1AndThenCallChainSubst(f,g,m)
          

   //def compose[F[_],A](f: Function[T1,R], m: CpsMonad[F])(g: A => F[T1]): A => F[R] =
   //     x => m.map(g(x))(f)


class Function1AndThenCallChainSubst[F[_],A,B,C](f: A=>B, g: B=>F[C], m:CpsMonad[F]) extends CallChainAsyncShiftSubst[F,A=>C, A=>F[C]]:

   def _origin: A=>F[C] = f.andThen(g)

   def apply(x:A): F[C] =
         g(f(x))

   def andThen[D](h: C=>D): Function1AndThenCallChainSubst[F,A,B,D] =
        Function1AndThenCallChainSubst(f, x => m.map(g(x))(h), m)
    
   def andThen_shifted[D](h: C=>F[D]): Function1AndThenCallChainSubst[F,A,B,D] =
        Function1AndThenCallChainSubst(f, x => m.flatMap(g(x))(h), m)



