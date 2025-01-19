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
import dotty.tools.dotc.transform.{Erasure, PureStats, VCElideAllocations, Inlining}
import dotty.tools.dotc.plugins.PluginPhase

/** Remove scaffolding from code after CpsChangeSymbols and before code generation
  */
trait RemoveScaffolding {

  this: PhaseChangeSymbolsAndRemoveScaffolding =>

  override def transformDefDef(tree: DefDef)(using Contexts.Context): Tree = {

    def reportErrorWithTree(msg: String, tree: Tree)(using Context): Unit = {
      report.error(msg, tree.srcPos)
      report.error(s"tree:  ${tree.show}", tree.srcPos)
      report.error(s"plain tree: ${tree}", tree.srcPos)
    }

    if (tree.symbol.is(Flags.Bridge)) then
      // search for bridge which is generated for CpsDirect methods
      //  (it created in erasure after uncarrying (preserve carrying form of method))
      tree.rhs match
        case treeBlock @ Block(List(ddef: DefDef), Closure(env, meth, tpe)) if meth.symbol == ddef.symbol =>
          ddef.rhs match
            case Apply(fn, args) =>
              fn.symbol.getAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsTransformed")) match
                case Some(transformedAnnotation) =>
                  ddef.tpe.widen match
                    case mt: MethodOrPoly =>
                      val nType = mt.derivedLambdaType(resType = ddef.rhs.tpe.widen)
                      // val nDdef = ddef.withType( mt.derivedLambdaType(resType = ddef.rhs.tpe.widen ))
                      // val nDdef = cpy.DefDef(ddef)(tpt = TypeTree(ddef.rhs.tpe.widen))
                      val newDdefSymbol = Symbols.newSymbol(ddef.symbol.owner, ddef.name, ddef.symbol.flags, nType)
                      val nDdef = DefDef(
                        newDdefSymbol,
                        paramss => {
                          val paramsMap = (ddef.paramss zip paramss).foldLeft(Map.empty[Symbol, Tree]) { case (s, (psOld, psNew)) =>
                            (psOld zip psNew).foldLeft(s) { case (s, (pOld, pNew)) =>
                              s.updated(pOld.symbol, ref(pNew.symbol).withSpan(pOld.span))
                            }
                          }
                          TransformUtil.substParamsMap(ddef.rhs, paramsMap)
                        }
                      )
                      cpy.DefDef(tree)(rhs = Block(List(nDdef), Closure(env, ref(newDdefSymbol), tpe)).withSpan(treeBlock.span))
                    case _ =>
                      throw CpsTransformException("Assumed that ddef.tpe.widen is MethodOrPoly", ddef.srcPos)
                case None =>
                  tree
        case _ =>
          tree
    else
      selectedNodes.getDefDefRecord(tree.symbol) match
        case Some(selectRecord) =>
          // if (true || selectRecord.debugLevel > 10) {
          //  log(s"changeSymbol for ${tree.symbol} ")
          //  log(s"rhs ${tree.rhs.show} ")
          //  log(s"plain rhs ${tree.rhs} ")
          // }
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
                CpsTransformHelper.cpsTransformedErasedType(tree.symbol.info, selectRecord.monadType, tree.srcPos)
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
            case EmptyTree =>
              tree
            case _ =>
              reportErrorWithTree(s"not found uncpsed scaffolding: for ${tree.symbol} (${tree.symbol.id})", tree.rhs)
              tree
        case None =>
          tree
  }

  override def transformApply(tree: Apply)(using ctx: Context): Tree = {

    tree match
      case Scaffolding.Cpsed(cpsedCall) => cpsedCall
      case _ =>
        tree
  }

  override def transformIdent(tree: Ident)(using Context): Tree = {
    if (tree.symbol.hasAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsTransformed"))) then
      ref(tree.symbol).withSpan(tree.span)
    else tree
  }

  def retrieveReturnType(ddefType: Type)(using Context): Type = {
    ddefType match
      case mt: MethodOrPoly =>
        mt.resType
      case _ =>
        report.error(s"not found return type for ${ddefType.show}")
        Types.NoType
  }

}
