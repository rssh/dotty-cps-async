package cps



trait CpsRuntimeAwait[F[_]] {

    def async[A,C <: CpsMonadContext[F]](f: C=>A)(m: CpsAsyncEffectMonad[F], ctx:C):F[A] = {
      m.flatDelay(runAsync(f)(m,ctx))
    }
    
    def runAsync[A,C <: CpsMonadContext[F]](f: C=>A)(m: CpsAsyncEffectMonad[F], ctx:C):F[A]

    def await[A](fa: F[A])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): A 

    def asyncFun0[R](f: ()=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): ()=>R = {
        () => this.await(f())(m,ctx)
    }

    def asyncFun1[A,R](f: A=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): A=>R = {
        a => this.await(f(a))(m,ctx)     
    }

    def asyncFun2[A,B,R](f: (A,B)=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): (A,B)=>R = {
        (a,b) => this.await(f(a,b))(m,ctx)     
    }

    def asyncFun3[A,B,C,R](f: (A,B,C)=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): (A,B,C)=>R = {
        (a,b,c) => this.await(f(a,b,c))(m,ctx)     
    }

    def asyncFun4[A,B,C,D,R](f: (A,B,C,D)=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): (A,B,C,D)=>R = {
      (a,b,c,d) => this.await(f(a,b,c,d))(m,ctx)       
    }

    def asyncFun5[A,B,C,D,E,R](f: (A,B,C,D,E)=>F[R])(m: CpsAsyncMonad[F], ctx: CpsMonadContext[F]): (A,B,C,D,E)=>R = {
      (a,b,c,d,e) => this.await(f(a,b,c,d,e))(m,ctx)       
    }

}


