package logic.unification1
import logic.unification1.UnificationSyntax.*
import cps.*
import logic.unification1.Unifiable.IntUnificable

import scala.compiletime.summonInline
import scala.reflect.ClassTag


trait Unifiable[T:ClassTag] {

  type Environment[_[+_]]

  type InstanceData

  def unify[R[+_]:Environment](value: T, term: LogicalTerm, bindings: Bindings)(using data: LogicEngineInstanceData[InstanceData]):R[Bindings]

  // TODO: reflec
  def classTag: ClassTag[T] =
     summon[ClassTag[T]]

  def checkType[S:Unifiable]: Boolean = {
    val s = summon[Unifiable[S]]
    classTag.runtimeClass.isAssignableFrom(s.classTag.runtimeClass)
  }

}

object Unifiable  {



  implicit object IntUnificable extends Unifiable[Int] {

    override type Environment[R[+_]] = SingleUnificationEnvironment[R]

    override type InstanceData = Any

    override def unify[R[+_]:Environment](value: Int, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[Any]): R[Bindings] = {
      term match
        case v@LogicalVariable(id, _) => bindings.bind(v, value)
        case t@LogicalConstant(c) =>
          c match
            case ci: Int => if (ci == value) success(bindings) else failure()
            case _ => failure()
        case CheckedLogicalTerm(t,cast, check) =>
          cast(value) match
            case Some(v) => if (check(v)) success(bindings) else failure()
            case None => failure()
        case _ => failure()
    }

  }


}


case class Point(x:Int, y:Int)

object Point {

  implicit object PointUnificable extends Unifiable[Point] {

    override type Environment[R[+_]] = UnificationEnvironment[R]

    override type InstanceData = Any
    
    def unify[R[+_]:UnificationEnvironment](value: Point, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[Any]): R[Bindings] = {
      term match
        case v@LogicalVariable(id, _) => bindings.bind(v, value)
        case LogicalConstant(c) =>
          c match
            case p: Point => if (p == value) success(bindings) else failure()
            case _ => failure()
        case CheckedLogicalTerm(t,cast, check) =>
          cast(value) match
            case Some(v) => if (check(v)) success(bindings) else failure()
            case None => failure()
        case LogicalFunctionSymbol(tUnifier,args) =>
          if (args.length != 2) {
            failure()
          } else {
            reify[R] {
              val x = args(0)
              val y = args(1)
              val intUnifier = summonInline[Unifiable[Int]]
              val xrBindings = reflect(intUnifier.unify(value.x, x, bindings))
              val yrBindings = reflect(intUnifier.unify(value.y, y, xrBindings))
              yrBindings
            }
          }
    }

  }

}