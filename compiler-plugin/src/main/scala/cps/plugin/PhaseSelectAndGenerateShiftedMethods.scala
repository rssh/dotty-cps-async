package cps.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*
import core.Decorators.toTermName
import plugins.*
import cps.plugin.DefDefSelectKind.USING_CONTEXT_PARAM
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.DenotTransformers.{DenotTransformer, IdentityDenotTransformer, InfoTransformer}
import dotty.tools.dotc.transform.{Pickler, SetRootTree}

class PhaseSelectAndGenerateShiftedMethods(selectedNodes: SelectedNodes) extends PluginPhase with IdentityDenotTransformer {

  val phaseName = PhaseSelectAndGenerateShiftedMethods.phaseName

  override val runsAfter = Set(SetRootTree.name)
  override val runsBefore = Set(Pickler.name, PhaseCps.name)

  override def changesMembers: Boolean = true
  override def changesParents: Boolean = true

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    tree.rhs match
      case EmptyTree =>
        tree
      case other =>
        if (
            !tree.symbol.flags.isOneOf(Flags.InlineOrProxy | Flags.Synthetic) &&
            CpsTransformHelper.isCpsDirectType(tree.rhs.tpe)
          )
        then
          report.error("CpsDirect can't be a value", tree.srcPos)
          tree
        else super.transformValDef(tree)
  }

  override def transformAssign(tree: tpd.Assign)(using Context): tpd.Tree = {
    if (CpsTransformHelper.isCpsDirectType(tree.tpe)) then
      report.error("CpsDirect can't be a assigned", tree.srcPos)
      tree
    else super.transformAssign(tree)
  }

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {

    // annotated selected methds with CpsTransform
    for (m <- tree.body) {
      val changed = annotateTopMethodWithSelectKind(m)
    }

    // add shifted methods for @makeCPS annotated high-order members
    val makeCpsAnnot = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    val shiftedMethods = tree.body
      .filter(_.symbol.annotations.exists(_.symbol == makeCpsAnnot))
      .flatMap { m =>
        m match
          case dd: DefDef => ShiftedMethodGenerator.generateShiftedMethod(dd)
          case other =>
            report.error("Only DefDef can be annotated by @makeCPS", other.srcPos)
            None
      }
    if (shiftedMethods.isEmpty) then tree
    else
      // despite this is not change signature,
      // tree.symbol.enteredAfter(this)
      // tree.symbol.asClass.enteredAfter(this)
      // tree
      for (m <- shiftedMethods) {
        m.symbol.enteredAfter(this)
      }
      cpy.Template(tree)(body = tree.body ++ shiftedMethods)
  }

  def annotateTopMethodWithSelectKind(tree: tpd.Tree)(using Context): Boolean = {
    lazy val cpsTransformedAnnot = Symbols.requiredClass("cps.plugin.annotation.CpsTransformed")
    tree match
      case dd: DefDef if (!dd.symbol.hasAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsNotChange"))) =>
        val optKind = SelectedNodes.detectDefDefSelectKind(dd)
        optKind match
          case Some(kind) =>
            val monadType =
              CpsTransformHelper.extractMonadType(kind.getCpsDirectContext.tpe, CpsTransformHelper.cpsDirectAliasSymbol, dd.srcPos)
            val annotExpr = New(cpsTransformedAnnot.typeRef.appliedTo(monadType), Nil)
            val initAnnotExpr = Apply(TypeApply(Select(annotExpr, "<init>".toTermName), List(TypeTree(monadType))), Nil)
            dd.symbol.addAnnotation(ConcreteAnnotation(initAnnotExpr))
            selectedNodes.addDefDef(dd.symbol, kind)
            true
          case None => false
      case _ =>
        false
  }

}

object PhaseSelectAndGenerateShiftedMethods {

  val phaseName = "rssh.cpsSelect"

}
