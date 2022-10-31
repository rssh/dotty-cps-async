package cps.macros.common

import scala.quoted._


class CheckedTreeConstructors(debugLevel: Int)  {

    class OwnerRecord(ctxs: Quotes)(val owner: qctx.reflect.Symbol, owned: List[qctx.reflect.Tree])  

    def Block(stats: List[Statement], last: Term)(using Quotes) {
      println("myBlock")
      val owners1 = collectOwners(stats)
      val owners2 = collectOwners(List(last))
      qctx.reflect.Block(stats,last)

    }


}