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

object IdentTransform {


      def apply(identTerm: Ident, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
          CpsTree.unchangedPure(tctx,identTerm,owner)
      }


}