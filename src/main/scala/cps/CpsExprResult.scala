package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._


case class CpsExprResult[F[_],T](
                origin:Expr[T],
                cpsBuild: CpsChunkBuilder[F,T],
                internalType: Type[T],
                haveAwait:Boolean
) {
    def transformed(given QuoteContext): Expr[F[T]] = cpsBuild.create().toExpr
}

    
