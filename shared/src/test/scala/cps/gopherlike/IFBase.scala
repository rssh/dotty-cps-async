package cps.gopherlike

import cps._

trait IFWriter[F[_],A]:

  var v:AnyRef = null

  protected def cpsMonad: CpsMonad[F]

  protected final def wCpsMonad: CpsMonad[F] = cpsMonad

  def awrite(a:A): F[Unit] =
    v = a.asInstanceOf[AnyRef]
    cpsMonad.pure(())

  inline def write(a:A): Unit =
    await[F,Unit](awrite(a))(using wCpsMonad)


class CIFWriter[F[_]:CpsMonad,A]  extends IFWriter[F,A]:
   protected override def cpsMonad = summon[CpsMonad[F]]


trait IFReader[F[_],A]:

   protected def value: Option[A]

   protected def cpsMonad: CpsMonad[F]

   def aread(): F[A] =
        cpsMonad.pure(value.get)

   inline def read():A =
        await(aread())(using cpsMonad)

   def foreach_async(f: A=> F[Unit]): F[Unit] =
         f(value.get)  

   // Think: mb create _internal_pure ??

   def aforeach_async(f: A=> F[Unit]): F[F[Unit]] =
         cpsMonad.pure(f(value.get))

   def aforeach(f: A=>Unit): F[Unit] = 
     given CpsMonad[F] = cpsMonad
     async{
         f(value.get)
     }

   inline def foreach(f: A=>Unit): Unit =
        await(aforeach(f))(using cpsMonad)

class CIFReader[F[_]:CpsMonad,A](a:A) extends IFReader[F,A]:

   override def value = Some(a)

   protected override def cpsMonad = summon[CpsMonad[F]]


trait IFChannel[F[_]:CpsMonad,A,B] extends IFWriter[F,A] with IFReader[F,B]:

   protected override def cpsMonad: CpsMonad[F]


class CIFChannel[F[_]:CpsMonad,A] extends IFChannel[F,A,A]:

   override def value = Some(v.asInstanceOf[A])

   protected override def cpsMonad = summon[CpsMonad[F]]

