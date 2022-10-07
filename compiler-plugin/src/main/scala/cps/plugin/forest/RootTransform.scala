package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import ast.tpd.*


object RootTransform {


      def apply(term: Tree, ctx: TransformationContext)(using Context): CpsTree = {
        term match
          case tIf@If(_,_,_) => IfTransform(tIf,ctx)
          case _ =>
            throw CpsTransformError(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)


      }

}