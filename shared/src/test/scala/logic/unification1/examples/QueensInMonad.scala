package logic.unification1.examples

import cps.*
import logic.unification1.*

import scala.reflect.ClassTag



class Pos(x:Int, y: Int) derives Fact



class Queens(positions: List[Pos], size: Int)

object Queens {

  given Unifiable[Queens] with

    override type Environment[R[+_]] = UnificationEnvironment[R]
    override type InstanceData = Any
    
    def unify[R[+_]:UnificationEnvironment](value: Queens, term: LogicalTerm, bindings: Bindings)(using LogicEngineInstanceData[Any]):R[Bindings] = reify[R]{
      ??? //TODO
    }


    def unifyNextQuens[R[+_]:UnificationEnvironment](xp: List[Pos], yp: List[Pos], k:Int, n:Int): R[Queens] = async[R] {
      ???
    }

}


