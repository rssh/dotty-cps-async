package logic.unification1.examples

import logic.unification1
import logic.unification1.*
import logic.unification1.DatalogSyntax.*

object FindPathes {

  case class Edge(x: Int, y:Int) derives Fact
  case class Path(x: Int, y:Int) derives Fact

  val rules = Datalog{
      (x: Int, y: Int, z: Int) => {
        Path(x, y) |-  Edge(x, y)
        Path(x, z) |-  Path(x, y) & Edge(y, z)
      }
  }

  val facts = Datalog{
       Edge(1,2) |- true
       Edge(2,3) |- true
       Edge(3,4) |- true
  }

  val facts2 = Prolog.asFacts(Path(1,2), Path(2,3), Path(3,4))

  val results = Prolog.defaultEngine(rules ++ facts).query(x => Path(1, x))

}
