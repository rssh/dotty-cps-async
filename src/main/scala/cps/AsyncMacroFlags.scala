package cps

import scala.quoted._

case class AsyncMacroFlags(
   printCode: Boolean = false,
   printTree: Boolean = false,
   debugLevel: Int = 0,
   allowShiftedLambda: Boolean = true,
   customValueDiscard: Boolean = false,
   warnValueDiscard: Boolean = true,
   muted: Boolean = false,
)

given Unliftable[AsyncMacroFlags]:
   def fromExpr(x: Expr[AsyncMacroFlags]) =
     x match
       case '{ AsyncMacroFlags(${Unlifted(x)},${Unlifted(y)},${Unlifted(z)}) } =>
                 Some(AsyncMacroFlags(x,y,z))
       case _ => None
