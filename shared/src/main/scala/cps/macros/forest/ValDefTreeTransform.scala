package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


trait ValDefTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import quotes.reflect.*

  def runValDefFromBlock(block: Block, valDef: ValDef)(owner: Symbol): CpsTree = {
    if cpsCtx.flags.debugLevel >= 15 then
      cpsCtx.log(s"ValDefTreeTransform:runValDefFomBlock, valDef=$valDef")
    val rhs = valDef.rhs.getOrElse(
            throw MacroError(s"val $valDef without right part in block ", block.asExpr)
    )
    //val rhsType = TransformUtil.veryWiden(rhs.tpe).asType
    val cpsRhs = runRoot(rhs)(valDef.symbol)
    ValCpsTree(owner,valDef, cpsRhs, CpsTree.empty, true)
  }

  
