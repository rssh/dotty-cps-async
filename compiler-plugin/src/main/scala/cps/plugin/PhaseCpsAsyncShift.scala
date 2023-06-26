package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Constants.*
import core.Annotations.*
import core.Decorators.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.TypeRef
import plugins.*
import transform.{ Erasure, Inlining, Pickler, PruneErasedDefs }

class PhaseCpsAsyncShift(selectedNodes: SelectedNodes, shiftedSymbols: ShiftedSymbols)
    extends PluginPhase {

  override val phaseName = PhaseCpsAsyncShift.name

  // strange -
  override def allowsImplicitSearch = true
  override val runsAfter            = Set(PhaseCps.name)
  override val runsBefore           = Set(Erasure.name, PhaseCpsAsyncReplace.name)

  // override def run(using Context): Unit = {
  // TODO:
  //  check what async-shift needed in the current compilation unit
  //   -- generate one in the special class
  //   -- update the global cache, setting as key - signature of function or method, value - async tree
  // }

  /**
   * looks for annotated functions, changes them and add to shiftedSymbols
   * @param tree
   * @param Context
   * @return
   */
  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    println(
      s"cpsAsyncShift::transformTemplate: ${tree.symbol.name}, ${tree.symbol.info.show}"
    )
    val annotationClass = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    var newMethods      = List.empty[DefDef]
    for (
      tree <- tree.body
      if tree.symbol.is(Flags.Method) /*&& isHightOrder() && generateCps */
    )
      tree match
        case fun: DefDef
            if (!fun.symbol.isAnonymousFunction &&
              !fun.symbol.denot.getAnnotation(annotationClass).isEmpty) =>
          val newFunName     = (fun.symbol.name.debugString + "$cps").toTermName
          val newFunSymbol   =
            Symbols.newSymbol(
              fun.symbol.owner,
              newFunName,
              fun.symbol.flags | Flags.Synthetic,
              fun.symbol.info // let be the same type for now
            )
          // create new rhs
          val ctx1: Context = summon[Context].withOwner(newFunSymbol)
          val transformedRhs = transformFunctionBody(fun.rhs)
          val nRhs           = Block(Nil, transformedRhs)(using ctx1)
          val newMethod      =
            DefDef(newFunSymbol, newParamss, fun.tpt.tpe, nRhs)

          // TODO: add to ShiftedSymbols
          shiftedSymbols.addAsyncShift(fun.symbol, newFunSymbol)
          newMethods = newMethod :: newMethods
        case _ => ()

    val retval = if (newMethods.isEmpty) {
      tree
    } else {
      // tree
      cpy.Template(tree)(body = tree.body ++ newMethods)
    }
    // super.transformTemplate(tree)
    // println(s"after CpsAsyncShift, retval: ${retval.show}")
    retval
  }

  /**
   * just print symbol info to check - is it changed in phascCps
   * @param tree
   * @param Context
   * @return
   */
  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree =
    // println(
    // s"cpsAsyncShift::transformDefDef: ${tree.symbol.name}, ${tree.symbol.info.show}"
    // )
    tree

  def changeOwner(param: Symbol, newOwner: Symbol)(using Context): Symbol =
    param.copy(owner = newOwner)

  def transformFunctionBody(tree: tpd.Tree): tpd.Tree =
    // println(s"tparams: ${tree.tparams}")
    // println(s"vparams: ${tree.vparamss}")
    tree

}

object PhaseCpsAsyncShift {
  val name: String = "rssh.cpsAsyncShift"
}
