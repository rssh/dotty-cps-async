package cps.plugin.forest

import dotty.tools.dotc.ast.tpd.{Tree, ValDef}
import dotty.tools.dotc.core.Contexts.Context


import cps.plugin.CpsTopLevelContext
import cps.plugin.forest.CpsTreeKindAccessor


sealed trait CpsTreeKindAccessor {
  def cpsTree: CpsTree
}

trait SyncCpsTreeAccessor extends CpsTreeKindAccessor {
  def getUnpure(using Context, CpsTopLevelContext): Tree =
    cpsTree.unpure.get
}

trait AsyncCpsTreeAccessor extends CpsTreeKindAccessor {
  def transformed(using Context, CpsTopLevelContext): Tree =
    cpsTree.transformed
}

trait LambdaCpsTreeAccessor extends CpsTreeKindAccessor {
  //def paramsTypes: List[ValDef]
  //def body: CpsTreeKindAccessor
}
