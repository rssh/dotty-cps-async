package logic.unification2.examples

import cps.*
import logic.*

import logic.unification2.*

case class Point(x: Int, y: Int)

object Point {

  given Unifiable[Point] with {

    override type Environment[M[_]] = UnificationEnvironment[M]

    override def unify[M[_]:Environment](a: M[Point], b: M[Point])(using Access[Any]): M[Point] = reify[M] {

      a.toTerm match
        case alv: LogicalVariable[Point] =>
           alv.bind(b)
        case aft: LogicalFunctionalTerm[Point] =>
           b.toTerm match
             case blv: LogicalVariable[Point] =>
                blv.bind(a)
             case bft: LogicalFunctionalTerm[Point] =>
                guard(aft.symbol == bft.symbol)
                val x = reflect(summon[Unifiable[Int]].unify(aft.args(0).castToM[Int], bft.args(0).castToM[Int]))
                val y = reflect(summon[Unifiable[Int]].unify(aft.args(1).castToM[Int], bft.args(1).castToM[Int]))
                Point(x,y)
    }

    override def fromTerm(term: LogicalTerm): Option[Point] = {
      term match
        case lft: LogicalFunctionalTerm[?] =>
          if lft.symbol == this then
            val x = summon[Unifiable[Int]].fromTerm(lft.args(0))
            val y = summon[Unifiable[Int]].fromTerm(lft.args(1))
            (x,y) match
              case (Some(x), Some(y)) => Some(Point(x,y))
              case _ => None
          else
            None
        case lc: LogicalConstant[?] =>
          if (lc.symbol == this) then
            lc.value match
              case p: Point => Some(p)
              case _ => None
          else
            None
        case lv: LogicalVariable[?] => 
          None
    }
    
    
    /*
    TODO: eable
    override def compare(x: Point, y: Point): Int = {
      x.x - y.x match
        case 0 => x.y - y.y
        case other => other
    }
    
     */

    override def className: String = classOf[Point].getCanonicalName

  }

}
