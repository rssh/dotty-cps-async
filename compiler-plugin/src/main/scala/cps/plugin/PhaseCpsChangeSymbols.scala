package cps.plugin

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.*
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.transform.{Erasure, PureStats, VCElideAllocations}
import dotty.tools.dotc.transform.TypeUtils.*
import dotty.tools.dotc.plugins.PluginPhase

class PhaseCpsChangeSymbols(selectedNodes: SelectedNodes, shiftedSymbols:ShiftedSymbols) extends PluginPhase, SymTransformer {

  override def phaseName: String = PhaseCpsChangeSymbols.name

  //override val runsAfter = Set(Erasure.name, PhaseCps.name, "rssh.cpsAsyncShift")
  //override val runsBefore = Set(PureStats.name, VCElideAllocations.name)

  override val runsAfter = Set(Erasure.name, PhaseCps.name)
  override val runsBefore = Set(VCElideAllocations.name)

  override def changesMembers: Boolean = true



  override def transformSym(sym: SymDenotations.SymDenotation)(using Context): SymDenotations.SymDenotation = {
    selectedNodes.getDefDefRecord(sym.symbol) match
      case Some(selectRecord) =>
        // we have erased type in sym.info
        //  normal full type saced
        println(s"transformSym for ${sym} (${sym.symbol.id}):  ${sym.info.show} ")
        val monadType = selectRecord.monadType
        val ntp = CpsTransformHelper.cpsTransformedErasedType(sym.info, monadType)
        //selectRecord.changedType = ntp
        println(s"transformSym: ${sym.info.show}  ->  ${ntp.show}, ${sym} '${sym.name.mangledString}' ${sym.symbol.id}")
        sym.copySymDenotation(info = ntp)
      case None =>
        sym
  }



  override def transformDefDef(tree: DefDef)(using Contexts.Context): Tree = {
    println(s"phashCpsChangeSymbols: transformDefDef:  ${tree.symbol} (${tree.symbol.id})) ")

    def reportErrorWithTree(msg: String, tree: Tree)(using Context): Unit = {
      report.error(msg, tree.srcPos)
      report.error(s"tree:  ${tree.show}", tree.srcPos)
      report.error(s"plain tree: ${tree}", tree.srcPos)
    }

    def removeScaffolding(tree: Tree)(using Context): Tree = {
      tree match
        case Apply(fn, List(arg)) =>
          if (Scaffolding.isAdoptForUncpsedDenotation(fn.symbol)) {
            arg
          } else {
            reportErrorWithTree("unexpected rhs for using-context-param: should be adoptedForUncpsedDenoation", tree)
            tree
          }
        case _ =>
          reportErrorWithTree("unexpected rhs for using-context-param: should be Apply", tree)
          tree
    }


    selectedNodes.getDefDefRecord(tree.symbol) match
      case Some(selectRecord) =>
        // here we see our defdefs with next changes:
        //  - erased type
        //  - type params are removed
        //  - all argument lists are merged into one
        //  - box/unbox for primitive types are inserted
        if (tree.tpt.tpe.isPrimitiveValueType || tree.tpt.tpe.isErasedValueType)
          tree.rhs match
            case Apply(cnUnbox, List(arg)) if Erasure.Boxing.isUnbox(cnUnbox.symbol) =>
              val nRhs = removeScaffolding(arg)
              val nTpe = CpsTransformHelper.cpsTransformedErasedType(tree.tpt.tpe, selectRecord.monadType)
              // TODO: set type by hand ?
              cpy.DefDef(tree)(rhs = nRhs, tpt = TypeTree(nTpe))
            case _ =>
              reportErrorWithTree("unexpected rhs for using-context-param: should be unbox", tree.rhs)
              tree
        else
          tree.rhs match
            case  oldLambda@Block((ddef: DefDef) :: Nil, closure: Closure) =>
              val nDefDef = transformDefDef(ddef)
              val nLambda = cpy.Block(tree.rhs)(nDefDef :: Nil, closure)
              cpy.DefDef(tree)(rhs = nLambda, tpt = TypeTree(nDefDef.tpe.widen))
            case _ =>
              val nRhs = removeScaffolding(tree.rhs)
              cpy.DefDef(tree)(rhs = nRhs, tpt = TypeTree(nRhs.tpe.widen))
      case None =>
        tree
  }

  override def transformApply(tree: Apply)(using Contexts.Context): Tree = {
    tree match
      case Apply(fn, List(arg)) =>
        // after erasure we have no TypeApply (TODO: check it)
        if (Scaffolding.isAdoptCpsedCall(fn.symbol)) {
          //  TODO: mb we should transform childs of arg.
          //  need to recheck, if transform is applied by defualt.
          //    arg type is changed in symbold denotation, but here compiler can yet not known about this,
          //     so let set new type manually.

          // TODO: for primitive types insert boxing/unboxing
          arg.withType(tree.tpe.widen)
        } else {
          super.transformApply(tree)
        }
      case _ =>
        tree
  }

}


object PhaseCpsChangeSymbols {
  val name = "rssh.cpsChangeSymbols"
}