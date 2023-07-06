package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Types.*
import ast.tpd.*



case class CpsTopLevelContext(
                               //val shiftedSymbols: ShiftedSymbols,
                               val monadType: Type, // F[_]
                               val cpsMonadValDef: ValDef, // val m = summon[CpsTryMonad[F]]
                               val cpsMonadRef: Tree, // m
                               val cpsMonadContextRef: Tree, // TODO: many contexts, if we have context per effect ?
                               val optRuntimeAwait: Option[Tree],
                               val optThrowSupport: Option[Tree],
                               val optTrySupport: Option[Tree],
                               val debugSettings: DebugSettings,
                               val pluginSettings: CpsPluginSettings,
                               val isBeforeInliner: Boolean,
)   {

  def isAfterInliner = !isBeforeInliner


}

