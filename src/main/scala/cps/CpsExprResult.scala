package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._


case class CpsExprResult[F[_],T](
                origin:Expr[T],
                cpsBuild: CpsChunkBuilder[F,T],
                originType: Type[T],
                haveAwait:Boolean
) {

    type MT[_] = F
    type TT = T

    def transformed(given QuoteContext): Expr[F[T]] = cpsBuild.create().toExpr
}

    
