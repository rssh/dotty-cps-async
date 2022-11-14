package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object RootTransform {


      def apply(term: Tree, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
        term match
          case block: Block => BlockTransform(block, owner, tctx)
          case tIf@If(_,_,_) => IfTransform(tIf, owner, tctx)
          case vd:ValDef => ValDefTransform(vd,owner,tctx)
          case _ =>
            throw CpsTransformException(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)


      }

}