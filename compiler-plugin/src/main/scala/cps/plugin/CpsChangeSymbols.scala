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
              case mt: MethodOrPoly if (!sym.isAnonymousFunction) =>
                findCpsDirectContextInParamss(sym.symbol, sym.rawParamss) match
                  case Some(monadType) =>
                    val ntp = CpsTransformHelper.cpsTransformedErasedType(sym.info, monadType)
                    sym.copySymDenotation(info = ntp)
                  case None =>
                    sym
              case _ =>
                sym
  }




  def findCpsDirectContextInParamss(sym: Symbol, paramss:List[List[Symbol]])(using ctx:Context): Option[Type] = {
    paramss.flatten.find(
      paramSym => {
        if (paramSym.isTypeParam  ||  !(paramSym.flags.isOneOf(Flags.GivenOrImplicit)))
          false
        else
          val retval = paramSym.info.widen.dealias match
            case vt: ErasedValueType =>
                 // for valueType
                (vt.tycon <:< Symbols.requiredClassRef("cps.CpsDirect"))
            case tr: TypeRef =>
                tr <:< Symbols.requiredClassRef("cps.CpsDirect")
            case _ =>
              false
          println(s"CpsChangeSymbols::checking CpsDirectContext for ${paramSym.info.widen.dealias}, result=${retval}")
          retval
      }
    ).map { paramSym =>
      val oldParamInfo = paramSym.current(using summon[Context].fresh.setPhase(firstTransformPhase)).info
      oldParamInfo match
        case AppliedType(tycon, args) =>
          val monadType = args.head
          monadType
        case _ =>
          report.warning("Unsupported type for CpsDirect param: ${oldParamInfo.show}, approximating to Wildcard")
          Types.WildcardType
    }
  }


}


