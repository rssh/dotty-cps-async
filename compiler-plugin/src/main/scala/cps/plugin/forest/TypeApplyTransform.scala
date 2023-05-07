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

import cps.plugin.*

object TypeApplyTransform {


      def apply(taTerm: TypeApply, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
          val funCps = RootTransform(taTerm.fun,owner, nesting+1)
          val newOp = SelectTypeApplyTypedCpsTree.OpTypeApply(taTerm)
          funCps match
            case SelectTypeApplyTypedCpsTree(records,nested,fcpsOrigin) =>
                SelectTypeApplyTypedCpsTree(records.appended(newOp),nested,taTerm)
            case _ =>
                val records = IndexedSeq(newOp)
                SelectTypeApplyTypedCpsTree(records,funCps,taTerm)
      }


}