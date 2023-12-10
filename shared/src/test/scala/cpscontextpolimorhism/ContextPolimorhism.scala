package cpscontextpolimorhism

import cps.*

trait AEnv[R[+_]] extends CpsTryMonad[R] {

  def a[A](v:R[A]):R[A]

}


trait BEnv[R[+_]] extends CpsTryMonad[R] {

  def b[A](v1:R[A],v2:R[A]):R[A]

}

trait CEEnv[R[+_]] extends CpsTryMonad[R] {

  def ce[A](v1:R[A],v2:R[A]):R[A]

}

type &&[A[_[+_]],B[_[+_]]] = [R[+_]] =>> A[R] & B[R]

trait Bindings
trait LogicalTerm
trait LogicEngineInstanceData[+D] {
  def get: D
}


trait Unifiable[T] {

  type C[_[+_]]
  type D

  def unify[R[+_]:C](v: T, t: LogicalTerm, b: Bindings)(using LogicEngineInstanceData[D]): R[Bindings]

}

object Unifiable {


}


case class ClientA(v:Int)

object ClientA {

  given Unifiable[ClientA] with

    type C[R[+_]] = AEnv[R]

    type D = Any

    override def unify[R[+_] : AEnv](v: ClientA, t: LogicalTerm, b:Bindings)(using LogicEngineInstanceData[D]): R[Bindings] = ???

}

trait ConnectionProvider {
  def connection: String
}

case class ClientB(v: Int)

object ClientB {

  given Unifiable[ClientB] with

    type C[R[+_]] = (AEnv && BEnv)[R]

    type D = ConnectionProvider


    override def unify[R[+_] : AEnv && BEnv](v: ClientB, t: LogicalTerm, b: Bindings)(using LogicEngineInstanceData[ConnectionProvider]): R[Bindings] = ???

}

trait IntProvider {
  def getInt: Int
}

case class ClientC(v: Int)

object ClientC {

  given Unifiable[ClientC] with

    type C[R[+_]] = (AEnv && BEnv && CEEnv)[R]

    type D = ConnectionProvider & IntProvider

    override def unify[R[+_] : AEnv && BEnv && CEEnv](v: ClientC, t: LogicalTerm, b: Bindings)
                                                     (using leid: LogicEngineInstanceData[ConnectionProvider & IntProvider]): R[Bindings] = reify[R]{
        val myInt = leid.get.getInt
        ???
    }

}


object ContextPolimorhism {


   def runner[R[+_]: AEnv && BEnv && CEEnv](ca:ClientA, cb: ClientB)
                                           (using id: LogicEngineInstanceData[ConnectionProvider & IntProvider]): R[Bindings] =
   reify[R]{
     val t: LogicalTerm = ???
     val b: Bindings = ???
     val s = id.get.connection
     val r1 = reflect(summon[Unifiable[ClientA]].unify[R](ca,t,b))
     val r2 = reflect(summon[Unifiable[ClientB]].unify[R](cb,t,r1))
     val r3 = reflect(summon[Unifiable[ClientC]].unify[R](ClientC(0),t,r2))
     r3
   }

}
