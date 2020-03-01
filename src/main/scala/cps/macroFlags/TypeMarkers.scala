package cps.macroFlags

import scala.quoted._
import scala.quoted.matching._

object PrintCode
object PrintTree

case class DebugLevel(value: Int)

given ValueOfExpr[DebugLevel]:
   
   def apply(x: Expr[DebugLevel])(using qctx: QuoteContext): Option[DebugLevel] =
     import qctx.tasty.{ _, given _ }
     println(s"ValueOfExpr[DebugLevel], x=${x.show}")
     x match 
       case '{ DebugLevel(${Value(x)}) } =>
                 Some(DebugLevel(x))
       case '{ new DebugLevel(${Value(x)}) } =>
                 Some(DebugLevel(x))
       case other => 
          val sym = other.unseal.symbol
          sym.tree match
             case ValDef(name,tpt,Some(rhs)) => 
                      apply(rhs.seal.asInstanceOf[Expr[DebugLevel]])
             case DefDef(name,tps,vps,rt,Some(rhs)) => 
                      apply(rhs.seal.asInstanceOf[Expr[DebugLevel]])
             case DefDef(name,tps,vps,tp,None) => 
                      // currently it is no-way to extract definition of given.
                      // TODO: submit request to dotty
                      None
             case other => 
                      println(s"other=${other.show}")
                      None
           
     

