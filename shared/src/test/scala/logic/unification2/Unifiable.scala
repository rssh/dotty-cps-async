package logic.unification2

import cps.*
import logic.CpsLogicMonad


trait Unifiable[T]  {

  type Environment[M[_]] <: UnificationEnvironment[M]

  type InstanceData = Any

  def unify[M[_]:Environment](a: M[T], b: M[T])(using data: Access[InstanceData]): M[T]
  
  def fromTerm(term: LogicalTerm): Option[T]

  def className: String

  lazy val classNameHash: Int = className.hashCode

  // don;t use Environment,  becase when we cast, we don;t have in code exact type of term
  // and can't deduce Environment.
  def  castTo[M[_]:UnificationEnvironment, S:Unifiable](t: LogicalTerm): M[TypedLogicalTerm[S]] =
    if (summon[Unifiable[S]] == this) then
      summon[UnificationEnvironment[M]].pure(t.asInstanceOf[TypedLogicalTerm[S]])
    else
      summon[UnificationEnvironment[M]].mzero

}

object Unifiable {

  given Ordering[Unifiable[?]] with
    def compare(x: Unifiable[?], y: Unifiable[?]): Int =
      x.classNameHash - y.classNameHash

  given Equiv[Unifiable[?]] with
    def equiv(x: Unifiable[?], y: Unifiable[?]): Boolean =
      x.className == y.className

  given Unifiable[Int] with
    type Environment[M[_]] = UnificationEnvironment[M]

    override def unify[M[_]:Environment](a: M[Int], b: M[Int])(using data: Access[InstanceData]): M[Int] = reify[M] {
      a.toTerm match
        case lc: LogicalConstant[?] => ???
          /*
          b.toTerm match
            case lc2: LogicalConstant[Int] =>
              if lc2 == lc then reflect(a) else failure[M,Int]
            case lv: LogicalVariable[Int] =>
              lv.bind(a.toTerm)
            case fun: FunctionalTerm[?] =>
              ???

           */

    }

    override def fromTerm(term: LogicalTerm): Option[Int] = term match
      case lc: LogicalConstant[?] =>
        if (lc.symbol == summon[Unifiable[Int]]) then
          Some(lc.value.asInstanceOf[Int])
        else
          None
      case _ => None

    //override def compare(x: Int, y: Int): Int = x - y

    override def className: String = "Int"


}