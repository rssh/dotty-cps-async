package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Types.*
import core.Contexts.*
import core.Decorators.*
import ast.tpd.*

case class CpsTopLevelContext(
    val monadType: Type, // F[_]
    // val cpsMonadValDef: ValDef, // val m = summon[CpsTryMonad[F]]
    val cpsMonadRef: Tree, // m
    val cpsDirectOrSimpleContextRef: Tree, // TODO: many contexts, if we have context per effect ?
    val optRuntimeAwait: Option[Tree],
    val optRuntimeAwaitProvider: Option[Tree],
    val optThrowSupport: Option[Tree],
    val optTrySupport: Option[Tree],
    val debugSettings: DebugSettings,
    val pluginSettings: CpsPluginSettings,
    val isBeforeInliner: Boolean
) {

  def isAfterInliner = !isBeforeInliner

  def supportsRuntimeAwait: Boolean =
    optRuntimeAwait.isDefined || optRuntimeAwaitProvider.isDefined

  def cpsNonDirectContext(using Context): Tree =
    if (CpsTransformHelper.isCpsDirectType(cpsDirectOrSimpleContextRef.tpe))
      Apply(
        TypeApply(ref(Symbols.requiredMethod("cps.CpsDirect.context")), List(TypeTree(monadType))),
        List(cpsDirectOrSimpleContextRef)
      )
    else
      cpsDirectOrSimpleContextRef

}
