package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._


case class CpsExprResult[F[_],T](
                origin:Expr[T],
                chunkBuilder: CpsChunkBuilder[F,T],
                originType: Type[T]
) {
    def isAsync = chunkBuilder.isAsync
    def transformed(given QuoteContext): Expr[F[T]] = chunkBuilder.create().toExpr
}

