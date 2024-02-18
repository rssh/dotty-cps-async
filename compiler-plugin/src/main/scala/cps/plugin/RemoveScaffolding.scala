package cps.plugin

import dotty.tools.dotc.ast.*
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
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.transform.{Erasure, PureStats, VCElideAllocations,Inlining}
import dotty.tools.dotc.plugins.PluginPhase


/**
 * Remove scaffolding from code after CpsChangeSymbols and before code generation
 */
trait RemoveScaffolding {

  this: PhaseChangeSymbolsAndRemoveScaffolding =>

  override def transformDefDef(tree: DefDef)(using Contexts.Context): Tree = {

    def reportErrorWithTree(msg: String, tree: Tree)(using Context): Unit = {
      report.error(msg, tree.srcPos)
      report.error(s"tree:  ${tree.show}", tree.srcPos)
      report.error(s"plain tree: ${tree}", tree.srcPos)
    }

    selectedNodes.getDefDefRecord(tree.symbol) match
      case Some(selectRecord) =>
        //if (true || selectRecord.debugLevel > 10) {
        //  log(s"changeSymbol for ${tree.symbol} ")
        //  log(s"rhs ${tree.rhs.show} ")
        //  log(s"plain rhs ${tree.rhs} ")
        //}
        // here we see our defdefs with next changes:
        //  - erased type
        //  - type params are removed
        //  - all argument lists are merged into one
        //  - box/unbox for primitive types are inserted
        tree.rhs match
          case Scaffolding.Uncpsed(nRhs) =>
            val changedDdefType = if (selectRecord.changedType != Types.NoType) {
              selectRecord.changedType
            } else {
              CpsTransformHelper.cpsTransformedErasedType(tree.symbol.info, selectRecord.monadType)
            }
            val nTpt = retrieveReturnType(changedDdefType)
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


  override def transformApply(tree: Apply)(using ctx: Context): Tree = {

    def retypeFn(fn: Tree):Tree = {
      fn match
        case id: Ident =>
          val retval = ref(id.symbol).withSpan(id.span)  // here this will be symbol after phase CpsChangeSymbols
          retval
        case sel: Select =>
          val retval = Select(sel.qualifier,sel.name).withSpan(sel.span)
          retval
        case _ =>
          fn
    }


    tree match
      case Scaffolding.Cpsed(cpsedCall) =>
        // here we need to retype arg because we change the type of symbols.
        val cpsedCallRetyped = cpsedCall match
          case Apply(fn, args) =>
            Apply(retypeFn(fn), args).withSpan(cpsedCall.span)
          case _ => cpsedCall
        cpsedCallRetyped
      case _ =>
        tree
  }



  def retrieveReturnType(ddefType: Type)(using Context): Type = {
    ddefType match
      case mt: MethodOrPoly=>
        mt.resType
      case _ =>
        report.error(s"not found return type for ${ddefType.show}")
        Types.NoType
  }


}

