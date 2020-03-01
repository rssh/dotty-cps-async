package cps

import scala.quoted._
import scala.quoted.matching._


case class AsyncMacroFlags(
   printCode: Boolean = false,
   printTree: Boolean = false,
   debugLevel: Int = 0,
   allowShiftedLambda: Boolean = false
)

given ValueOfExpr[AsyncMacroFlags]:
   
   def apply(x: Expr[AsyncMacroFlags])(using qctx: QuoteContext): Option[AsyncMacroFlags] =
     x match 
       case '{ AsyncMacroFlags(${Value(x)},${Value(y)},${Value(z)}) } =>
                 Some(AsyncMacroFlags(x,y,z))
       case _ => None
           
     

