package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Types.*
import ast.tpd.*



case class CpsTopLevelContext(
  //val shiftedSymbols: ShiftedSymbols,
  val monadType: Type,  // F[_]
  val cpsMonadRef: Tree,  // summon[CpsTryMonad[F]]
  val cpsMonadContextRef: Tree, // TODO: many contexts, if we have context per effect ?
  val optRuntimeAwait: Option[Tree] ,
  val settings: DebugSettings
)  

