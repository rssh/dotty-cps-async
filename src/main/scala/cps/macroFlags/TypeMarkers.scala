package cps.macroFlags

import scala.quoted._

object PrintCode
object PrintTree

case class DebugLevel(value: Int)

given Unliftable[DebugLevel]:
   
   def apply(x: Expr[DebugLevel])(using qctx: QuoteContext): Option[DebugLevel] =
     import qctx.tasty.{ _, given _ }
     println(s"Unliftable[DebugLevel], x=${x.show}")
     x match 
       case '{ DebugLevel(${Unlifted(x)}) } =>
                 Some(DebugLevel(x))
       case '{ new DebugLevel(${Unlifted(x)}) } =>
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
           
     

