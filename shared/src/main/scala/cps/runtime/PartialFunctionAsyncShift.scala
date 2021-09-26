package cps.runtime

import cps._


trait PartialFunctionCallChainSubst[F[+_],A,B](m:CpsMonad[F]) extends 
                                                         CallChainAsyncShiftSubst[F,PartialFunction[A,B], A=>F[B]]:

   fThis =>

   import PartialFunctionCallChainSubst._

   def apply(x:A):F[B]

   def isDefinedAt(x:A):F[Boolean]

   def applyContOrElse[A1 <:A, C](x: A1, ifApply:B=>C, ifNot: A1=>C): F[C]

   def applyOrElse[A1 <:A, B1 >: B](x: A1, default: (A1)=>B1): F[B1] =
                               applyContOrElse(x, identity, default)

   def applyContOrElse_async[A1 <:A, C](x: A1, ifApply:B=>F[C], ifNot: A1=>F[C]): F[C]

   def applyOrElse_async[A1 <:A, B1 >: B](x: A1, default: (A1)=>F[B1]): F[B1] =
                               applyContOrElse_async(x, x => m.pure(x), default)

   def _finishChain: A=>F[B] = (x => apply(x))

   def lift: CallChainAsyncShiftSubst[F,A=>Option[B], A=>F[Option[B]]]  =
      Function1AndThenCallChainSubst[F,A,A,Option[B]](identity,
          (x:A) => applyContOrElse[A,Option[B]](x, (y:B)=>Some(y), _ => None),
          m)

   def andThen[C](g: PartialFunctionCallChainSubst[F,B,C]): PartialFunctionCallChainSubst[F,A,C] =
     val r = new PartialFunctionCallChainSubst[F,A,C](m) {
        def apply(x:A):F[C] = m.flatMap(fThis.apply(x))(g.apply)
        def isDefinedAt(x:A):F[Boolean] = 
              m.flatMap(fThis.isDefinedAt(x))( fd =>
                 if (fd) then m.flatMap(fThis.apply(x))(g.isDefinedAt) else m.pure(false) 
              )

        def applyContOrElse[A1 <:A, D](x:A1, ifApply: C=>D, ifNot: A1=>D): F[D]=
             fThis.applyContOrElse_async(x,
                     y => g.applyContOrElse(y, ifApply, _ => ifNot(x) ),
                     x1 => m.pure(ifNot(x1))
             )

        def applyContOrElse_async[A1 <:A, D](x:A1, ifApply: C=>F[D],  ifNot: A1=>F[D]): F[D]=
             fThis.applyContOrElse_async(x,
                     y => g.applyContOrElse_async(y, ifApply, _ => ifNot(x) ),
                     ifNot
             )

     }
     r

   def andThen[C](g: B=>C): PartialFunctionCallChainSubst[F,A,C] =
         fThis.andThen(totalPlain(m,g))

   def andThen[C](g: PartialFunction[B,C]): PartialFunctionCallChainSubst[F,A,C] =
         fThis.andThen(partialPlain(m,g))

   def andThen_async[C](g: B => F[C]): PartialFunctionCallChainSubst[F,A,C] =
         fThis.andThen(totalMapped(m,g))

   def andThen_async[C](g: PartialFunction[B,F[C]]): PartialFunctionCallChainSubst[F,A,C] =
         fThis.andThen(partialMapped(m,g))

   def compose[Z](g: PartialFunction[Z,A]): PartialFunctionCallChainSubst[F,Z,B] =
         partialPlain(m,g).andThen(fThis)

   def compose[Z](g: Z=>A): PartialFunctionCallChainSubst[F,Z,B] =
         totalPlain(m,g).andThen(fThis)

   def compose_async[Z](g: PartialFunction[Z,F[A]]): PartialFunctionCallChainSubst[F,Z,B] =
         partialMapped(m,g).andThen(fThis)

   def compose_async[Z](g: Z=>F[A]): PartialFunctionCallChainSubst[F,Z,B] =
         totalMapped(m,g).andThen(fThis)

   def orElse[A1 <: A, B1 >: B](g: PartialFunctionCallChainSubst[F,A1,B1]): PartialFunctionCallChainSubst[F,A1,B1] =
     new PartialFunctionCallChainSubst[F,A1,B1](m) {
        def apply(x:A1):F[B1] = 
          m.flatMap(fThis.isDefinedAt(x)){ fr =>
             if (fr) fThis.apply(x) else g.apply(x)
          }

        def isDefinedAt(x:A1):F[Boolean] =
          m.flatMap(fThis.isDefinedAt(x)){ frs =>
             if (frs) m.pure(true) else g.isDefinedAt(x)
          }

        override def applyOrElse[A2 <:A1, B2 >: B1](x:A2, default: A2=>B2): F[B2]=
          fThis.applyOrElse_async[A2,B2](x, x=>g.applyOrElse[A2,B2](x,default) )
                                              

        def applyContOrElse[A2<:A1, C](x:A2, ifApply: B1=>C, ifNot: A2=>C): F[C]=
              fThis.applyContOrElse_async[A2,C](x, 
                        b1 => m.pure(ifApply(b1)),
                        x2 => g.applyContOrElse[A2,C](x2, ifApply, ifNot)
              )

        override def applyOrElse_async[A2 <:A1, B2 >: B1](x:A2, default: A2=>F[B2]): F[B2]=
          fThis.applyOrElse_async(x, x => g.applyOrElse_async(x,default))

        def applyContOrElse_async[A2 <:A1, C](x:A2, ifApply: B1=>F[C], ifNot: A2=>F[C]): F[C]=
              fThis.applyContOrElse_async[A2,C](x, 
                      b1 => ifApply(b1),
                      x2 => g.applyContOrElse_async[A2,C](x2, ifApply, ifNot)
              )
          
     }




object PartialFunctionCallChainSubst:

   def totalPlain[F[+_],A,B](m:CpsMonad[F], f: A=>B): PartialFunctionCallChainSubst[F,A,B] =
     new PartialFunctionCallChainSubst[F,A,B](m) {
        override def apply(x:A):F[B] = m.pure(f(x))
        override def isDefinedAt(x:A):F[Boolean] = m.pure(true)
        override def applyOrElse[A1 <:A, B1 >: B](x:A1, default: A1=>B1): F[B1] = apply(x)
        override def applyContOrElse[A1 <:A, C](x:A1, ifApply: B=>C,  ifNot: A1=>C): F[C] = 
              m.pure(ifApply(f(x)))
        override def applyOrElse_async[A1 <:A, B1 >: B](x:A1, default: A1=>F[B1]): F[B1] = 
              apply(x)
        override def applyContOrElse_async[A1 <:A, C](x:A1, ifApply: B=>F[C], ifNot: A1=>F[C]): F[C] = 
              ifApply(f(x))
     }

   def partialPlain[F[+_],A,B](m: CpsMonad[F], f: PartialFunction[A,B]): PartialFunctionCallChainSubst[F,A,B] =
     new PartialFunctionCallChainSubst[F,A,B](m) {
        override def apply(x:A):F[B] = m.pure(f(x))
        override def isDefinedAt(x:A):F[Boolean] = m.pure(f.isDefinedAt(x))
        override def applyOrElse[A1 <:A, B1 >: B](x:A1, default: A1=>B1): F[B1] = 
             m.pure(f.applyOrElse(x,default))
        override def applyContOrElse[A1 <:A, C](x:A1, ifApply: B=>C, ifNot: A1=>C): F[C] = 
             f.lift.apply(x) match
                case Some(y) => m.pure(ifApply(y))
                case None => m.pure(ifNot(x))
        override def applyOrElse_async[A1 <:A, B1 >: B](x:A1, default: A1=>F[B1]): F[B1] = 
             f.lift.apply(x) match
                case Some(y) => m.pure(y)
                case None => default(x)
        override def applyContOrElse_async[A1 <:A, C](x:A1, ifApply: B=>F[C], ifNot: A1=>F[C]): F[C] = 
             f.lift.apply(x) match
                case Some(y) => ifApply(y)
                case None => ifNot(x)
     }

   def totalMapped[F[+_],A,B](m: CpsMonad[F], f: A=>F[B]): PartialFunctionCallChainSubst[F,A,B] =
     new PartialFunctionCallChainSubst[F,A,B](m) {
        override def apply(x:A):F[B] = f(x)
        override def isDefinedAt(x:A):F[Boolean] = m.pure(true)
        override def applyOrElse[A1 <:A, B1 >: B](x:A1, default: A1=>B1): F[B1] = apply(x)
        override def applyContOrElse[A1 <:A, C](x:A1, ifApply: B=>C, ifNot: A1=>C): F[C] = 
            m.map(f(x))(ifApply)
        override def applyContOrElse_async[A1 <:A, C](x:A1, ifApply: B=>F[C], ifNot: A1=>F[C]): F[C] = 
            m.flatMap(f(x))(ifApply)
     }

   def partialMapped[F[+_],A,B](m: CpsMonad[F], f: PartialFunction[A,F[B]]): PartialFunctionCallChainSubst[F,A,B] =
     new PartialFunctionCallChainSubst[F,A,B](m) {
        override def apply(x:A):F[B] = f(x)
        override def isDefinedAt(x:A):F[Boolean] = m.pure(f.isDefinedAt(x))
        override def applyOrElse[A1 <:A, B1 >: B](x:A1, default: A1=>B1): F[B1] = 
              f.applyOrElse[A1,F[B1]](x, a => m.pure(default(a)) )
        override def applyContOrElse[A1 <:A, C](x:A1, ifApply: B=>C, ifNot: A1=>C): F[C] = 
              f.lift(x) match
                 case Some(fy) => m.map(fy)(ifApply)
                 case None => m.pure(ifNot(x))
        override def applyContOrElse_async[A1 <:A, C](x:A1, ifApply: B=>F[C], ifNot: A1=>F[C]): F[C] = 
              f.lift(x) match
                 case Some(fy) => m.flatMap(fy)(ifApply)
                 case None => ifNot(x)
     }



trait PartialFunctionAsyncShiftBase[T,R, C <: PartialFunction[T,R]] extends AsyncShift[C] :
                                        
   import PartialFunctionCallChainSubst._

   def andThen[F[+_],A](f: PartialFunction[T,R], m: CpsMonad[F])(g: (R) => F[A]): PartialFunctionCallChainSubst[F,T,A] =
        g match
          case gp: PartialFunction[R,F[A]] =>
            partialPlain(m,f).andThen(partialMapped(m,gp))
          case _ =>
            partialPlain(m, f).andThen(totalMapped(m,g))
          

   //def andThen[F[+_],A](f: PartialFunction[T,R], m: CpsMonad[F])(g: PartialFunction[R,F[A]]): 
   //                                                                   PartialFunctionCallChainSubst[F,T,A] =
   //     partialPlain(m,f).andThen(partialMapped(m,g))

   def compose[F[+_],A](f: PartialFunction[T,R], m: CpsMonad[F])(g: A => F[T]): PartialFunctionCallChainSubst[F,A,R] =
        totalMapped(m,g) andThen partialPlain(m,f)

   def applyOrElse[F[_], A1<:T, B1>:R](f: PartialFunction[T,R], m: CpsMonad[F])(x1: A1, default: (A1)=>F[B1]): F[B1] =
         f.lift.apply(x1) match
             case Some(b) => m.pure(b)
             case None => default(x1)

   //def runWith[F[_], U](f: PartialFunction[T,R], m: CpsMonad[F])(action: (R)=>U): A=>F[B1] = ???


class PartialFunctionAsyncShift[T,R] extends PartialFunctionAsyncShiftBase[T,R, PartialFunction[T,R]] 

