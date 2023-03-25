package cps.plugin

import scala.annotation.tailrec

import dotty.tools.dotc.*
import core.*
import core.Annotations.*
import core.Decorators.*
import core.Contexts.*
import core.Constants.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import plugins.*


case class DebugSettings(
  debugLevel: Int,
  printTree: Boolean,
  printCode: Boolean
)

object DebugSettings {


  def make(from:Tree)(using Context):DebugSettings = {
    val debugLevelAnSym = Symbols.requiredClass("cps.plugin.annotation.CpsDebugLevel")
    val debugLevel: Int = findAnnotation(summon[Context].owner, debugLevelAnSym) match
      case Some(an) =>
        an.argument(0) match
          case Some(Literal(Constant(v:Int))) => v
          case other => 
            throw CpsTransformException(s"CpsDebugLevelAnnotation should have literal constant as argument, we have $other",an.tree.srcPos)
      case None =>
        0
    if (false) {
      //don't  work after 'cc' stage  (always show name)
      //TODO: look after inlinging or ask user to enable retain-tree
      val debugLevelTpe = Symbols.requiredClass("cps.plugin.settings.DebugLevel").typeRef
      val debugLevel = CpsTransformHelper.findImplicitInstance(debugLevelTpe,from.span) match
        case Some(debugLevelTree) =>  // shoule be
          debugLevelTree.symbol.defTree match
            case f: DefDef =>
              f.rhs match
                case Typed(Apply(_,List(Literal(Constant(v:Int)))),_) => v
                case other =>
                  throw CpsTransformException(s"Invalid DebugLevel settings, see: ${f.rhs.show}",debugLevelTree.srcPos)
            case _ =>
              println(s"DebugLevel: debugLevel.symbol.tree=${debugLevelTree.symbol.defTree}")
              throw CpsTransformException(s"Invalid DebugLevel settings, expected constant application, see: ${debugLevelTree.show}",debugLevelTree.srcPos)
        case None => 
          println(s"DebugLevel instance is not found at ${from.span}")
          0  
    }
    val printCodeTpe = Symbols.requiredClass("cps.plugin.settings.PrintCode").typeRef
    val printCode  = CpsTransformHelper.findImplicitInstance(printCodeTpe, summon[Context].tree.span).isDefined
    val printTreeTpe = Symbols.requiredClass("cps.plugin.settings.PrintTree").typeRef
    val printTree  = CpsTransformHelper.findImplicitInstance(printTreeTpe, summon[Context].tree.span).isDefined
    DebugSettings(debugLevel = debugLevel, printTree = printTree, printCode = printCode)
  }


  @tailrec
  def findAnnotation(current:Symbol, annotationSymbol:Symbol)(using Context):Option[Annotation] = {
    if (current != NoSymbol) {
      current.denot.getAnnotation(annotationSymbol) match
        case Some(annotation) => Some(annotation)
        case None => findAnnotation(current.owner, annotationSymbol)
    } else None
  }

}
