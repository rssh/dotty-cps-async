package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Symbols.*
import core.Decorators.*
import core.Definitions.*
import core.StdNames
import ast.tpd.*

object TypeApplyTransform {


      def apply(taTerm: TypeApply, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
          val funCps = RootTransform(taTerm.fun,owner,tctx)
          val newOp = SelectTypeApplyCpsTree.OpTypeApply(taTerm)
          funCps match
            case SelectTypeApplyCpsTree(records,nested,fcpsOrigin,fcpsOriginOwner) =>
                SelectTypeApplyCpsTree(records.appended(newOp),nested,taTerm,owner)
            case _ =>
                val records = IndexedSeq(newOp)
                SelectTypeApplyCpsTree(records,funCps,taTerm,owner)
      }


}