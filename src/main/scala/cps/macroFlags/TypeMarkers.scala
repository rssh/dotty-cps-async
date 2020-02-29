package cps.macroFlags

import scala.quoted._
import scala.quoted.matching._

object PrintCode
object PrintTree

case class DebugLevel(level: Int)

given ValueOfExpr[DebugLevel]:
   
   def apply(x: Expr[DebugLevel])(using qctx: QuoteContext): Option[DebugLevel] =
     x match 
       case '{ DebugLevel(${Value(x)}) } =>
                 Some(DebugLevel(x))
       case _ => None
           
     

