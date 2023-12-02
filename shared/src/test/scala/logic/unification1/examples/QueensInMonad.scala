package logic.unification1.examples

import cps.*
import logic.unification1.*

import scala.reflect.ClassTag



class Pos(x:Int, y: Int) derives Fact



class Queens(positions: List[Pos], size: Int)

object Queens {

  given Unifiable[Queens] with

    override def classTag: ClassTag[Queens] = summon[ClassTag[Queens]]

    def unify[R[+_]:UnificationEnvironment,D](value: Queens, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[D]):R[Bindings] = reify[R]{
      ??? //TODO
    }


    def unifyNextQuens[R[+_]:UnificationEnvironment](xp: List[Pos], yp: List[Pos], k:Int, n:Int): R[Queens] = async[R] {
      ???
    }

}


