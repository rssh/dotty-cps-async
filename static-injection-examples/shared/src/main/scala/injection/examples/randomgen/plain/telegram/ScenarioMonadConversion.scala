package injection.examples.randomgen.plain.telegram

import canoe.api.Scenario
import cps.CpsMonadConversion

class ScenarioMonadConversion[F[_]] extends CpsMonadConversion[F, [X] =>> Scenario[F, X]] {
  override def apply[T](ft: F[T]): Scenario[F, T] = Scenario.eval(ft)
}

given [F[_]]: ScenarioMonadConversion[F] = ScenarioMonadConversion[F]
