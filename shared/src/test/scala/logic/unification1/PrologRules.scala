package logic.unification1

import scala.annotation.compileTimeOnly
import scala.quoted.*

trait PrologSyntax {

   case class SyntaxHornClause[A,B] (head:A, body:B)

   extension [A](head:A)
     @compileTimeOnly("SyntaxHornClause should only be used within a Prolog program.")
     def |-[B] (body:B): Unit = ???

     def &[B] (body:B): SyntaxHornClause[A,B] = ???

   @compileTimeOnly("SyntaxHornClause should only be used within a Prolog program.")
   def where[A:Type,B:Type] (head:A, body:B): Unit = ???


}

object PrologSyntax extends PrologSyntax
object DatalogSyntax extends PrologSyntax

case class PrologProgram(variables:List[LogicalVariable], clauses:List[HornClause]) {

    def ++(other:PrologProgram):PrologProgram = {
      //variables can have the same names, but they will be with different source positions.
      PrologProgram(variables ++ other.variables, clauses ++ other.clauses)
    }

}


case class PrologQuery(variables:List[LogicalVariable], expression:LogicalExpression)

trait PrologEngine {
  def apply(program: PrologProgram): PrologEngine

  type R[_]

  def queryExpression(qe: PrologQuery):R[Bindings]

  inline def query[A](inline body:A): R[Bindings] =
    queryExpression(translate(body))

  inline def translate[A](inline body:A): PrologQuery = ${
    PrologEngine.translateQueryImpl('body)
  }

}

object PrologEngine {

  def translateQueryImpl[A:Type](body:Expr[A])(using Quotes):Expr[PrologQuery] = {
    println(s"received query: ${body.show}")
    //TODO: implement
    '{ PrologQuery(Nil, LogicalTrue) }
  }

}

object Prolog extends PrologSyntax {

  inline def apply[A](inline body:A): PrologProgram = ${applyImpl[A]('body)}

  def applyImpl[A:Type](body:Expr[A])(using Quotes):Expr[PrologProgram] = {
    println(s"received body: ${body.show}")
    // TODO: immplement.
    '{PrologProgram(Nil,Nil)}
  }

  def asFacts[A:Fact](facts: A *): PrologProgram = {
    val hornClauses = facts.map(_.asHornClause).toList
    PrologProgram(Nil,hornClauses)
  }


  extension[T: Fact] (t: T) {
    def asHornClause: HornClause = summon[Fact[T]].asHornClause(t)
  }

  def defaultEngine: PrologEngine = ???

}

object Datalog extends PrologSyntax {

  inline def apply[A](inline body:A): PrologProgram = ${
    Prolog.applyImpl[A]('body)
  }

}