package cps.injection.examples.randomgen.provider

import cps.injection.examples.randomgen.PureEffect

class StringRandomProvider(using RandomProvider[PureEffect, Int]) extends RandomProvider[PureEffect, String] {
  def provideMany(n: Int): PureEffect[Iterable[String]] = {
    for {
      ri <- summon[RandomProvider[PureEffect, Int]].provideMany(n)
    } yield ri.map(util.Random.nextString)
  }
  def provideOne: PureEffect[String] = {
    for {
      ri <- summon[RandomProvider[PureEffect, Int]].provideOne
    } yield util.Random.nextString(ri)
  }
}