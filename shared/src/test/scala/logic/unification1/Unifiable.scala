package logic.unification1
import  logic.unification1.UnificationSyntax.*

import cps.*
import scala.reflect.ClassTag

trait Unifiable[T] {


  def unify[R[+_]:UnificationEnvironment,D](value: T, term: LogicalTerm, bindings: Bindings)(using data:LogicEngineInstanceData[D]):R[Bindings]

  // TODO: reflec
  def classTag: ClassTag[T]

  def checkType[S:Unifiable]: Boolean = {
    val s = summon[Unifiable[S]]
    classTag.runtimeClass.isAssignableFrom(s.classTag.runtimeClass)
  }

}

object Unifiable  {



  implicit object IntUnificable extends Unifiable[Int] {

    def classTag = summon[ClassTag[Int]]

    def unify[R[+_]:UnificationEnvironment,D](value: Int, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[D]): R[Bindings] = {
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

    override def classTag: ClassTag[Point] = summon[ClassTag[Point]]
    
    def unify[R[+_]:UnificationEnvironment,D](value: Point, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[D]): R[Bindings] = {
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
              val xUnifier = implicitly[Unifiable[Int]]
              val yUnifier = implicitly[Unifiable[Int]]
              val xrBindings = reflect(xUnifier.unify(value.x, x, bindings))
              val yrBindings = reflect(yUnifier.unify(value.y, y, xrBindings))
              yrBindings
            }
          }
        case _ => failure()
    }

  }

}