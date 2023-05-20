package cps.plugin

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.*
import dotty.tools.dotc.core.Definitions.*
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.transform.{Erasure, PureStats, VCElideAllocations}
import dotty.tools.dotc.transform.TypeUtils.*
import dotty.tools.dotc.plugins.PluginPhase

import scala.collection.immutable.Nil

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
        selectRecord.changedType = ntp
        println(s"transformSym: ${sym.info.show}  ->  ${ntp.show}, ${sym} '${sym.name.mangledString}' ${sym.symbol.id}")
        sym.copySymDenotation(info = ntp)
      case None =>
        sym
  }


  object UncpsedScaffolding {

    def unapply(tree: Tree)(using Context): Option[Tree] = {
      tree match
        case TypeApply(sel@Select(internal, asInstanceOfCn), List(tpt))
          if (asInstanceOfCn.toString == "asInstanceOf") =>
          internal match
            case UncpsedScaffolding(internal1) =>
              Some(cpy.TypeApply(tree)(Select(internal1, asInstanceOfCn), List(TypeTree(internal1.tpe.widen))))
            case _ =>
              None
        case Apply(cnUnbox, List(internal)) if Erasure.Boxing.isUnbox(cnUnbox.symbol) =>
          internal match
            case UncpsedScaffolding(internal1) =>
              println(s"found unbox, internal1.tpe.widen = ${internal1.tpe.widen.show}")
              // TODO: set asInstanceOf ?
              Some(internal1)
            case _ => None
        case Block((ddef: DefDef) :: Nil, closure: Closure) =>
          ddef.rhs match
            case UncpsedScaffolding(nRhs) =>
              Some(cpy.Block(tree)(cpy.DefDef(ddef)(rhs = nRhs, tpt = TypeTree(nRhs.tpe.widen)) :: Nil, closure))
            case _ =>
              None
        case Apply(fn, List(arg)) if (Scaffolding.isAdoptForUncpsedDenotation(fn.symbol)) =>
          Some(arg)
        case Block(List(UncpsedScaffolding(internal)),Literal(Constant(()))) =>
          // we can have function Direct => Unit and it's can be wrapped in empty block to return Unit regardless of computation result.
          Some(internal)
        case _ => None
    }

  }


  override def transformDefDef(tree: DefDef)(using Contexts.Context): Tree = {
    println(s"phashCpsChangeSymbols: transformDefDef:  ${tree.symbol} (${tree.symbol.id})) ")

    def reportErrorWithTree(msg: String, tree: Tree)(using Context): Unit = {
      report.error(msg, tree.srcPos)
      report.error(s"tree:  ${tree.show}", tree.srcPos)
      report.error(s"plain tree: ${tree}", tree.srcPos)
    }

    selectedNodes.getDefDefRecord(tree.symbol) match
      case Some(selectRecord) =>
        //if (true || selectRecord.debugLevel > 10) {
        //  println(s"changeSymbol for ${tree.symbol} ")
        //  println(s"rhs ${tree.rhs.show} ")
        //  println(s"plain rhs ${tree.rhs} ")
        //}
        // here we see our defdefs with next changes:
        //  - erased type
        //  - type params are removed
        //  - all argument lists are merged into one
        //  - box/unbox for primitive types are inserted
        tree.rhs match
          case UncpsedScaffolding(nRhs) =>
            println(s"found uncpsed scaffolding for ${tree.symbol} (${tree.symbol.id})")
            println(s"nRhs.tpe = ${nRhs.tpe.widen.show}")
            val changedDdefType = if (selectRecord.changedType != Types.NoType) {
              selectRecord.changedType
            } else {
              CpsTransformHelper.cpsTransformedErasedType(tree.symbol.info, selectRecord.monadType)
            }
            val nTpt = retrieveReturnType(changedDdefType)
            println(s"nTpt = ${nTpt.show}")
            val typedNRhs = if (nRhs.tpe.widen <:< nTpt) {
              nRhs
            } else {
              // we know that monads are not primitive types.
              //  (potentially, we can have monadic value classes in future)
              TypeApply(Select(nRhs, "asInstanceOf".toTermName), List(TypeTree(nTpt)))
            }
            // TODO: insert asInstanceOf ?
            cpy.DefDef(tree)(rhs = typedNRhs, tpt = TypeTree(nTpt))
          case _ =>
            reportErrorWithTree(s"not found uncpsed scaffolding: for ${tree.symbol} (${tree.symbol.id})", tree.rhs)
            tree
      case None =>
        tree
  }

  object CpsedScaffolding {

    def unapply(tree: Tree)(using Context): Option[Tree] = {
      tree match
        case Apply(fn, List(arg)) if (Scaffolding.isAdoptCpsedCall(fn.symbol)) =>
          // TODO:  (are we need check for isInstanceOf here?)
          if (true) {
            println(s"uncpsedScaffFolding:arg= ${arg.show}")
            println(s"uncpsedScaffFolding:plain tree = ${arg}")
          }
          arg match
            case Apply(fn1, List(arg1)) if Erasure.Boxing.isBox(fn1.symbol) =>
              Some(arg1)
            case Block(List(stat),expr) if expr == Literal(Constant(()))
                                  || expr.symbol.exists && expr.symbol == defn.BoxedUnit_UNIT =>
              Some(stat)
            case _ =>
              Some(arg)
        case _ => None
    }


  }

  override def transformApply(tree: Apply)(using Contexts.Context): Tree = {
    tree match
      case CpsedScaffolding(arg) =>
          arg.withType(tree.tpe.widen)
      case _ =>
          super.transformApply(tree)
  }

  def retrieveReturnType(ddefType: Type)(using Context): Type = {
    ddefType match
      case mt: MethodType =>
        mt.resType
      case _ =>
        report.error(s"not found return type for ${ddefType.show}")
        Types.NoType
  }

}


object PhaseCpsChangeSymbols {
  val name = "rssh.cpsChangeSymbols"
}