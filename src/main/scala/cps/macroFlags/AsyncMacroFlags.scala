package cps.macroFlags

import scala.quoted._
import scala.quoted.matching._

object PrintCode
object PrintTree

case class AsyncMacroFlags(
   printCode: Boolean = false,
   printTree: Boolean = false,
   debugLevel: Int = 1,
   exprMarker: String = ""
)

given ValueOfExpr[AsyncMacroFlags]:
   
   def apply(x: Expr[AsyncMacroFlags])(using qctx: QuoteContext): Option[AsyncMacroFlags] =
     x match 
       case '{ AsyncMacroFlags(${Value(x)},${Value(y)},${Value(z)},${Value(w)}) } =>
                 Some(AsyncMacroFlags(x,y,z,w))
       case _ => None
           
     

