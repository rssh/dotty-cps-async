package logic.unification1.examples

import logic.unification1.*
import logic.unification1.UnificationSyntax.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*, given}
import cps.stream.*

import scala.reflect.ClassTag


object FakeDB {

  trait ConnectionProvider {
    def connection: Connection
  }

  trait Connection {
    def collection[T](name: String): Collection[T]
  }

  trait Collection[T] {
    def find(query: Map[String,Any]): List[T]
    def findOne(query: Map[String,Any]): Option[T]
    def asyncFind[F[_]](): AsyncList[F,T]
  }

}

object DBAccess1 {

  case class User(id:Long, name: String, email: String, phoneNumber: String, score: Int)

  object User {
    import FakeDB._
    
    val userFact = Fact.derived[User]

    given Unifiable[User] with

      override type Environment[R[+_]] = UnificationEnvironment[R]

      override type InstanceData = ConnectionProvider

      override def unify[R[+_] : UnificationEnvironment](value: User, term: LogicalTerm, bindings:Bindings)(using instanceData: LogicEngineInstanceData[ConnectionProvider]): R[Bindings] = {
        val connection = instanceData.get.connection
        term match
          case lv:LogicalVariable =>
            // retrieva all the users from the database.
            val users:List[User] = connection.collection[User]("users").find(Map.empty)
            disjunction(users.map(u=>bindings.bind(lv,u)(using userFact)) *)
          case lc@LogicalConstant(value) =>
            connection.collection[User]("users").findOne(Map{ "id" -> value }) match
              case Some(userInDb) => userFact.unify[R](userInDb, lc, bindings)
              case None => failure()
          case LogicalFunctionSymbol(unifiable, args) =>
            if (unifiable.checkType[User]) then
              val query = Map.empty ++ args(0).ground.map{  "id" -> _ } ++ args(1).ground.map{  "name" -> _ } ++
                          args(2).ground.map{  "email" -> _ } ++ args(3).ground.map{  "phoneNumber" -> _ }
              val users = connection.collection[User]("users").find(query)
              or(users.map(u => userFact.unify[R](u, term, bindings)))
            else
              failure()
          case CheckedLogicalTerm(term, cast, check) =>
            ???
      }

  }


}

import scala.concurrent.*

object DBAccess2 {

  case class User(id: Long, name: String, email: String, phoneNumber: String, score: Int)

  object User {

    import FakeDB._

    val userFact = Fact.derived[User]

    trait UnificationAndDelayEnvironment[R[+_]] extends UnificationEnvironment[R] with DelayUnificationEnvironment[R]

    given Unifiable[User] with

      override type Environment[R[+_]] = (AsyncUnificationEnvironment.AuxF[Future] && DelayUnificationEnvironment)[R]
      override type InstanceData = ConnectionProvider

      override def unify[R[+_] : Environment](value: User, term: LogicalTerm, bindings: Bindings)(using instanceData: LogicEngineInstanceData[ConnectionProvider]): R[Bindings] = reify[R] {
        val connection = instanceData.get.connection
        reflect(waitResolved(term)) match
          case lv: LogicalVariable =>
            // v was not resolved.
            reflect(logicError("Term for all users should be resolved"))
          case lc@LogicalConstant(value) =>
            connection.collection[User]("users").findOne(Map("id" -> value)) match
              case Some(userInDb) => reflect(userFact.unify[R](userInDb, lc, bindings))
              case None => reflect(failure())
          case LogicalFunctionSymbol(unifiable, args) =>
            if (unifiable.checkType[User]) then
              val query = Map.empty ++ args(0).ground.map {
                "id" -> _
              } ++ args(1).ground.map {
                "name" -> _
              } ++
                args(2).ground.map {
                  "email" -> _
                } ++ args(3).ground.map {
                "phoneNumber" -> _
              }
              val users = connection.collection[User]("users").find(query)
              reflect(or(users.map(u => userFact.unify[R](u, term, bindings))))
            else
              reflect(failure())
          case CheckedLogicalTerm(term, cast, check) =>

              ???

      }


  }


}

object DBAccess3 {

  import FakeDB._

  case class User(id: Long, name: String, email: String, phoneNumber: String, score: Int) derives Fact

  case class QueryAllUsers(u: User)

  object QueryAllUsers {

    given Unifiable[QueryAllUsers] with

      override type Environment[R[+_]] = AsyncUnificationEnvironment.AuxF[Future][R]
      override type InstanceData = ConnectionProvider

      override def unify[R[+_] : AsyncUnificationEnvironment.AuxF[Future] ](value: QueryAllUsers , term: LogicalTerm, bindings: Bindings)(using instanceData: LogicEngineInstanceData[ConnectionProvider]): R[Bindings] = reify[R] {
        val collection = instanceData.get.connection.collection[User]("users")
        term match
          case lv: LogicalVariable =>
            reflect(asyncOr(collection.asyncFind().map(u => bindings.bind(lv, QueryAllUsers(u)))))
          case _ =>
            ??? // logic similar to the previous example
      }

  }

}