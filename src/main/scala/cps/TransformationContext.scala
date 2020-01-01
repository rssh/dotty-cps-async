package cps

import scala.quoted._
import scala.quoted.matching._


class TransformationContext[F[_],T](
                 given qctx:QuoteContext  
   )(
   patternCode: Expr[T],  // code, for which we build pattern expression
   originCode: qctx.tasty.Term,
   unpattern: qctx.tasty.Term => qctx.tasty.Tree,
   dm: AsyncMonad[F],
   tType: Type[T]
)

