package cps

import scala.quoted._

case class AsyncMacroFlags(
   printCode: Boolean = false,
   printTree: Boolean = false,
   debugLevel: Int = 0,
   allowShiftedLambda: Boolean = true
)

given Unliftable[AsyncMacroFlags]:
   
   def apply(x: Expr[AsyncMacroFlags])(using qctx: QuoteContext): Option[AsyncMacroFlags] =
     x match 
       case '{ AsyncMacroFlags(${Unlifted(x)},${Unlifted(y)},${Unlifted(z)}) } =>
                 Some(AsyncMacroFlags(x,y,z))
       case _ => None
           
     

