package logic.unification1

import scala.quoted._
import scala.reflect.ClassTag

trait Fact[T] extends Unifiable[T] {
  def asHornClause(t:T): HornClause  = HornClause(LogicalConstant[T](t)(using this), IndexedSeq.empty)
}

object Fact {

  inline def derived[T]: Fact[T] = ${ deriveFact[T] }

  def deriveFact[T:Type](using Quotes): Expr[Fact[T]] =
    import quotes.reflect._
    Expr.summon[ClassTag[T]] match
      case Some(ct) =>
        '{
            new Fact[T] {
              def classTag: ClassTag[T] = $ct
              def unify[R[+_]:UnificationEnvironment,D](value: T, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[D]):R[Bindings] = ??? // TODO
            }
          }
      case None => report.error("No ClassTag found for type T"); ???
}
