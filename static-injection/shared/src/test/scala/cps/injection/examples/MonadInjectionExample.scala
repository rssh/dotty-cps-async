package cps.injection.examples


import cps.*
import cps.injection.{*,given}


trait DB:
  def query(q: String): String

trait Console:
  def readLine(): String
  def printLine(s: String): Unit


trait DBImpl extends DB:
  def query(q: String): String = s"query: $q"


class MyModule:

  class InjectCall[T] {

    transparent inline def getM[F[_],S](using AsyncInjectionCarrier[F,S], Contains[T,S], CpsMonadContext[F]): T =
      await(asyncGetM)

    def asyncGetM[F[_],S](using AsyncInjectionCarrier[F,S], Contains[T,S] ): F[T] =
      summon[AsyncInjectionCarrier[F,S]].get[T]

    def get[S](using InjectionCarrier[S], Contains[T,S]): T = summon[InjectionCarrier[S]].get[T]

  }
  def inject[T]: InjectCall[T] = new InjectCall[T]

  def run1[F[_]:AsyncInject[DB]:CpsTryMonad](): F[Unit] = async[F] {
    val db = inject[DB].getM
    println("query result: "+db.query("select * from table"))
  }


  def run2[F[_]:AsyncInject[DB *: Console *: EmptyTuple]:CpsTryMonad](): F[Unit] = async[F] {
    val db = inject[DB].getM
    val console = inject[Console].getM
    println("query result: "+db.query("select * from table"))
  }

  def run1d(using InjectionCarrier[DB]): Unit = {
    val db = inject[DB].get
    println("query result: "+db.query("select * from table"))
  }

end MyModule


