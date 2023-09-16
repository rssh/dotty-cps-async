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
import dotty.tools.dotc.core.Phases.{firstTransformPhase, inliningPhase, postTyperPhase}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeErasure.ErasedValueType
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.transform.{Erasure, PureStats, VCElideAllocations}
import dotty.tools.dotc.transform.TypeUtils.*
import dotty.tools.dotc.plugins.PluginPhase



trait CpsChangeSymbols {

  this: PhaseChangeSymbolsAndRemoveScaffolding =>

  /**
   * Change symbols of direct context functions.
   *
   *  F: (args, include given CpsDirect[F]):T
   *  =>
   *  F: (args, include given CpsDirect[F]): F[T]
   */
  override def transformSym(sym: SymDenotations.SymDenotation)(using Context): SymDenotations.SymDenotation = {
    selectedNodes.getDefDefRecord(sym.symbol) match
      case Some(selectRecord) =>
        // we have erased type in sym.info
        //  normal full type saced
        val monadType = selectRecord.monadType
        val ntp = CpsTransformHelper.cpsTransformedErasedType(sym.info, monadType)
        selectRecord.changedType = ntp
        sym.copySymDenotation(info = ntp)
      case None =>
            sym.info match
              case mt0: MethodOrPoly if (!sym.isAnonymousFunction) =>
                val timeTravelContext = summon[Context].fresh.setPhase(firstTransformPhase)
                val cpsDirectSym = CpsTransformHelper.cpsDirectAliasSymbol(using timeTravelContext)
                val oldSym = sym.current(using timeTravelContext)
                oldSym.info match
                  case mtf: MethodOrPoly =>
                    println(s"transformSym: search for context param in ${mtf.show}")
                    findCpsDirectContextInParamss(oldSym.symbol, mtf, timeTravelContext) match
                      case Some(contextParamType) =>
                        val monadType = CpsTransformHelper.extractMonadType(contextParamType, cpsDirectSym, oldSym.symbol.srcPos)(using timeTravelContext)
                        println(s"transformSym: found context param in ${mtf.show} with monadType=${monadType.show}")
                        val ntp = CpsTransformHelper.cpsTransformedErasedType(sym.info, monadType)
                        sym.copySymDenotation(info = ntp)
                      case None =>
                        println(s"transformSym: not found context param in ${mtf.show}")
                        sym
                  case _ =>
                    sym
              case _ =>
                sym
  }




  def findCpsDirectContextInParamss(sym: Symbol, mtf: MethodOrPoly, timeTravelContext: Context)(using ctx:Context): Option[Type] = {
    println(s"findCpsDirectContextInParamss: search in ${mtf.show}")
    mtf match
      case pt: PolyType =>
        pt.resType match
          case mtf1: MethodOrPoly =>
            findCpsDirectContextInParamss(sym, mtf1, timeTravelContext)
          case _ =>
            None
      case mt: MethodType =>
        val firstFound:Option[Type] =
          if (!mt.isContextualMethod && !mt.isImplicitMethod) then
            println(s"findCpsDirectContextInParamss: skip ${mt.show} as not contextual or implicit")
            None
          else
            println(s"findCpsDirectContextInParamss: seach in ${mt.paramInfos}")
            mt.paramInfos.find(t => CpsTransformHelper.isCpsDirectType(t, debug = true)(using timeTravelContext))
        val paramFound = firstFound.orElse {
          mtf.resType match
            case mt: MethodOrPoly =>
              findCpsDirectContextInParamss(sym, mt, timeTravelContext)
            case _ => None
        }
        paramFound
   }


}


