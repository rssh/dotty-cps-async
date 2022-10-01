package cps.plugin

import scala.annotation.*

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import ast.tpd.*
import plugins.*


class PhaseCps(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "cps"

  override val runsAfter = Set("cc")
  override val runsBefore = Set("cpsAsyncShift")

  

  override def transformDefDef(tree:DefDef)(using Context): Tree = {
    // TODO:
    //  find parameter with outer value CpsTransform[F] ?=> T
    //  Translate to function which return M[T] instead T which body is cps-transformed
    if (Symbols.defn.isContextFunctionType(tree.tpt.tpe)) then
       println(s"defDef: name=${tree.name}, tpt=${tree.tpt}")
       tree.tpt match
          case tt: TypeTree =>
            println(s"isTypeTree, tt=${tt} ")
            tt.typeOpt match
              case AppliedType(tycon, args) =>
                println(s"AppliedType, args=$args")
                findCpsTransformInContextFunctionArgs(args) match
                  case Some(cpsTransformType) => 
                    println(s"found transfomed type $cpsTransformType")
                  case _ =>
                    println(s"not cps")
              case _  =>
                println(s"not AppliedType")
          case _ =>
            println(s"is not applied-type=tree but ${tree.tpt} ")
    tree
  }


  @tailrec
  private def findCpsTransformInContextFunctionArgs(args:List[Type])(using Context):Option[Type] =
      val classSym = Symbols.requiredClass("cps.E.CpsTransform")
      args match
        case h::Nil => None 
        case h::t =>
          println(s"findCpsTransformInContextFunctionArgs::check $h")
          h match
            case AppliedType(tycon, targs) if (tycon.typeSymbol == classSym) =>
              println(s"findCpsTransformInContextFunctionArgs::found h: $h")
              Some(h)
            case _ =>
              findCpsTransformInContextFunctionArgs(t)
        case Nil =>
          None

}

