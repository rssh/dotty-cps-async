package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object ValDefTransform {


      def apply(term: ValDef, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
       
            throw CpsTransformException(s"ValDef is not implemented: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)


      }

}