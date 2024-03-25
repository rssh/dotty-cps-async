package logic.unification1

import scala.quoted._
import scala.reflect.ClassTag

trait Fact[T] extends Unifiable[T] {

  override type Environment[R[+_]] = UnificationEnvironment[R]
  override type InstanceData = Any

  def asHornClause(t:T): HornClause  = HornClause(LogicalConstant[T](t)(using this), IndexedSeq.empty)
}

object Fact {

  inline def derived[T]: Fact[T] = ${ deriveFact[T] }

  def deriveFact[T:Type](using Quotes): Expr[Fact[T]] =
    import quotes.reflect._
    Expr.summon[ClassTag[T]] match
      case Some(ct) =>
        '{ given ClassTag[T] = $ct
            new Fact[T] {
              override def unify[R[+_]:UnificationEnvironment](value: T, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[Any]):R[Bindings] = ??? // TODO
            }
          }
      case None => report.error("No ClassTag found for type T"); ???
}
