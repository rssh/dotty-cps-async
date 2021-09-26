package cps.runtime

import cps._


class Function1AsyncShift[T,R] extends AsyncShift[Function1[T,R]]:

   def andThen[F[_],A](f: T=>R, m: CpsMonad[F])(g: (R) => F[A]): Function1AndThenCallChainSubst[F,T,R,A] =
        Function1AndThenCallChainSubst(f,g,m)
          

   def compose[F[_],A](f: Function[T,R], m: CpsMonad[F])(g: A => F[T]): Function1ComposeCallChainSubst[F,T,R,A] =
        Function1ComposeCallChainSubst(f,g,m)


class Function1AndThenCallChainSubst[F[_],A,B,C](f: A=>B, g: B=>F[C], m:CpsMonad[F]) extends CallChainAsyncShiftSubst[F,A=>C, A=>F[C]]:

   def _finishChain: A=>F[C] = f.andThen(g)

   def apply(x:A): F[C] =
         g(f(x))

   def andThen[D](h: C=>D): Function1AndThenCallChainSubst[F,A,B,D] =
        Function1AndThenCallChainSubst(f, x => m.map(g(x))(h), m)
    
   def andThen_async[D](h: C=>F[D]): Function1AndThenCallChainSubst[F,A,B,D] =
        Function1AndThenCallChainSubst(f, x => m.flatMap(g(x))(h), m)

   def compose[Z](h: Z => A): Function1AndThenCallChainSubst[F,Z,B,C] =
        Function1AndThenCallChainSubst( x => f(h(x)), g, m)

   def compose_async[Z](h: Z => F[A]): Function1ComposeAndThenCallChainSubst[F,Z,A,C] =
        Function1ComposeAndThenCallChainSubst(h, (x:A)=>g(f(x)), m)



class Function1ComposeCallChainSubst[F[_],A,B,Z](f: A=>B, g: Z=>F[A], m:CpsMonad[F]) extends CallChainAsyncShiftSubst[F,Z=>B, Z=>F[B]]:

   def _finishChain: Z=>F[B] = (z => m.map(g(z))(f))

   def apply(x:Z): F[B] = 
        m.map(g(x))(f)

   def andThen[C](h: B=>C): Function1ComposeCallChainSubst[F,A,C,Z] =
        Function1ComposeCallChainSubst((x:A)=>h(f(x)), g, m)

   def andThen_async[C](h: B=>F[C]): Function1ComposeAndThenCallChainSubst[F,Z,A,C] =
        Function1ComposeAndThenCallChainSubst(g, f andThen h, m)

   def compose[Y](h: Y => Z): Function1ComposeCallChainSubst[F,A,B,Y] =
        Function1ComposeCallChainSubst(f, h andThen g, m)

   def compose_async[Y](h: Y => F[Z]): Function1ComposeCallChainSubst[F,A,B,Y] =
        Function1ComposeCallChainSubst(f, y => m.flatMap(h(y))(g), m)


class Function1ComposeAndThenCallChainSubst[F[_],A,B,C](f: A=>F[B], g:B => F[C], m:CpsMonad[F]) extends 
                                                                                     CallChainAsyncShiftSubst[F,A=>C, A=>F[C]]:

   def _finishChain: A=>F[C] = (a => m.flatMap(f(a))(g))

   def apply(x:A): F[C] = 
          m.flatMap(f(x))(g)

   def andThen[D](h: C=>D): Function1ComposeAndThenCallChainSubst[F,A,B,D] =
        Function1ComposeAndThenCallChainSubst(f, (x:B)=> m.map(g(x))(h), m)

   def andThen_async[D](h: C=>F[D]): Function1ComposeAndThenCallChainSubst[F,A,B,D] =
        Function1ComposeAndThenCallChainSubst(f, (x:B)=> m.flatMap(g(x))(h), m)

   def compose[Z](h: Z => A): Function1ComposeAndThenCallChainSubst[F,Z,B,C] =
        Function1ComposeAndThenCallChainSubst(h andThen f, g, m)

   def compose_async[Z](h: Z => F[A]): Function1ComposeAndThenCallChainSubst[F,Z,B,C] =
        Function1ComposeAndThenCallChainSubst((x:Z)=>m.flatMap(h(x))(f), g, m)




