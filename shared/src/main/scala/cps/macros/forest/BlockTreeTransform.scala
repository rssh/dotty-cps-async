package cps.macros.forest

import scala.quoted._
import scala.util.control.NonFatal

import cps._
import cps.macros._
import cps.macros.misc._
import cps.macros.common._

/**
 * BlockTreeTransform -- the same as BlockTransform but on term level.
 * (yet not enabled)
 **/
trait BlockTreeTransform[F[_],CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._  

  def runBlock(block: Block, prevs: List[Statement], last: Term)(owner:Symbol): CpsTree = {
    if (prevs.isEmpty) then
      runRoot(last)(owner)
    else
      val prevsCpsTrees = prevs.map{
        case d: Definition =>
            d match
              case v@ValDef(vName,vtt,optRhs) => runValDefFromBlock(block, v)(owner)
              case _ => PureCpsTree(owner,d)
        case t: Term =>
            runRoot(t)(owner)
        case i: Import => CpsTree.empty
        case other => 
          throw MacroError(s"unknown tree type in block: $other", block.asExpr)
      }
      val lastCps = runRoot(last)(owner)
      val prevsCps: CpsTree = prevsCpsTrees.foldLeft(CpsTree.empty){ (s,e) => s.append(e) }
      prevsCps.appendFinal(lastCps)
  }




