package cps.macros

import scala.quoted._
import misc._

case class AsyncMacroFlags(
   printCode: Boolean = false,
   printTree: Boolean = false,
   debugLevel: Int = 0,
   allowShiftedLambda: Boolean = true,
   customValueDiscard: Boolean = false,
   warnValueDiscard: Boolean = true,
   automaticColoring: Boolean = false,
   muted: Boolean = false
)

given FromExpr[AsyncMacroFlags] with

   def unapply(v: Expr[AsyncMacroFlags])(using Quotes): Option[AsyncMacroFlags] =
     v match
       case '{ AsyncMacroFlags(${Expr(printCode)},${Expr(printTree)},
                               ${Expr(debugLevel)},
                               ${Expr(allowShiftedLambda)}, 
                               ${Expr(customValueDiscard)},
                               ${Expr(warnValueDiscard)}, 
                               ${Expr(automaticColoring)},
                               ${Expr(eMuted)}
                              ) 
                           } =>
               Some(AsyncMacroFlags(printCode, printTree,
                       debugLevel,
                       allowShiftedLambda,
                       customValueDiscard,
                       warnValueDiscard,
                       automaticColoring,
                       eMuted,
                   ))
       case _ => None


