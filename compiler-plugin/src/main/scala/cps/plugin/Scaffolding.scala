package cps.plugin

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import ast.{Trees, tpd}
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import cps.plugin
import util.SrcPos
import plugins.*
import cps.plugin.QuoteLikeAPI.*


/**
 * Generate adapoters fro the cps-transformed function be able to comply non-cpsed type in symbol denoatations,
 * because we can't change symbol denoatations before erasure phase.
 */
object Scaffolding {

  def adoptUncpsedRhs(tree: Tree, tType: Type,  fType:Type)(using Context): Tree = {
    val adoptSymbol = Symbols.requiredMethod("cps.plugin.scaffolding.adoptForUncpsedDenotation")
    val adoptIdent = ref(adoptSymbol)
    val adoptedTree = Apply(TypeApply(adoptIdent, List(TypeTree(fType), TypeTree(tType.widen))), tree :: Nil)
    adoptedTree
  }

  def adoptCpsedCall(tree: Tree, origType: Type, fType: Type)(using Context): Tree = {
    val adoptSymbol = Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")
    val adoptIdent = ref(adoptSymbol)
    Apply(TypeApply(adoptIdent, List(TypeTree(fType), TypeTree(origType))), tree :: Nil)
  }

  def isAdoptForUncpsedDenotation(sym: Symbol)(using Context): Boolean = {
      sym == Symbols.requiredMethod("cps.plugin.scaffolding.adoptForUncpsedDenotation")
  }

  def isAdoptCpsedCall(sym: Symbol)(using Context): Boolean = {
       sym == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")
  }

}

