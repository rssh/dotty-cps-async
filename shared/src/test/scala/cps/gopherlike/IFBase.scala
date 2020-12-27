package cps.gopherlike

import cps._

class IFWriter[F[_]:CpsMonad,A]:

  var v:AnyRef = null

  def awrite(a:A): F[Unit] =
    v = a.asInstanceOf[AnyRef]
    summon[CpsMonad[F]].pure(())

  inline def write(a:A): Unit =
    await[F,Unit](awrite(a))


class IFReader[F[_]:CpsMonad,A](a:A):


   def foreach_async(f: A=> F[Unit]): F[Unit] =
         f(a)  

   // Think: mb create _internal_pure ??

   def aforeach_async(f: A=> F[Unit]): F[F[Unit]] =
         summon[CpsMonad[F]].pure(f(a))

   def aforeach(f: A=>Unit): F[Unit] = async{
         f(a)
   }

   inline def foreach(f: A=>Unit): Unit =
        await(aforeach(f))

