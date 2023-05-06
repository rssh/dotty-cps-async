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

object IdentTransform {


      def apply(identTerm: Ident,  oldOnwer: Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
          CpsTree.unchangedPure(identTerm.changeOwner(oldOnwer,newOwner),newOwner)
      }


}