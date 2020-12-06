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

/*
given FromExpr[AsyncMacroFlags] with

   def unapply(v: Expr[AsyncMacroFlags]): Option[AsyncMacroFlags] =
     v match
       case '{ AsyncMacroFlags($ePrintCode,$ePrintTree,$eDebugLevel,
                               $eAllowShiftedLambda, $eCustomValueDiscard,
                               $eWarnValueDiscard, $eMuted) } =>
          val printCode = ePrintCode match
            case Expr(x) => x
            case _ =>
               throw MacroError("printCode should be a constant expression", ePrintCode)
          ??? 
               //Some(AsyncMacroFlags(printCode, 
               //     ))
       case _ => None
*/


