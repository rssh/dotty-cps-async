package cps.macros.flags

import scala.quoted._

/**
 * if implicit object of type PrintCode.type is defined, than macro print code befroe and after expansion 
 **/
class PrintCode
object PrintCode extends PrintCode


/**
 * if implicit of type PrintTree.type is defined, them macro print  ast tree before and after macro transformation.
 **/
class PrintTree
object PrintTree extends PrintTree

/**
 * if implicit object is defined, than macro use runtime await for CpsRuntimeAwait
 *  (work only on JVM with JDK-19 early access, which inclipe project loom support)
 **/
class UseLoomAwait
object UseLoomAwait extends UseLoomAwait

/**
 * if implicit object is defined, than macro delegate cps transformation to compiler plugin.
 **/
class UseCompilerPlugin
object UseCompilerPlugin extends UseCompilerPlugin


/**
 * Use this flag for debugging of async transformation process.
 *
 * ```
 *     implicit val = cps.macros.flags.DebugLevel(10)
 *     async{ 
 *        ....
 *     }
 * ```
 *  will output ton of traces during macro translation.
 *
 * @param value - debug level, from 0 to 20.  0 - no debug outout, 20 - maximum  
 **/
case class DebugLevel(value: Int)

object DebugLevel:

   given FromExpr[DebugLevel] with

      def unapply(x: Expr[DebugLevel])(using Quotes) =
         import quotes.reflect._
         x match
            case '{ DebugLevel(${Expr(x)}) } =>
                 Some(DebugLevel(x))
            case '{ new DebugLevel(${Expr(x)}) } =>
                 Some(DebugLevel(x))
            case other =>
               val sym = other.asTerm.symbol
               sym.tree match
                  case ValDef(name,tpt,Some(rhs)) =>
                      unapply(rhs.asExprOf[DebugLevel])
                  case DefDef(name,params,rt,Some(rhs)) =>
                      unapply(rhs.asExprOf[DebugLevel])
                  case DefDef(name,params,tp,None) =>
                      // currently it is no-way to extract definition of given.
                      // TODO: submit request to dotty
                      None
                  case other =>
                      println(s"unliftable cps.macros.flags.debugLevel ${other.show}")
                      None

end DebugLevel


