package cps.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*
import plugins.*
import cps.plugin.DefDefSelectKind.USING_CONTEXT_PARAM
import dotty.tools.dotc.core.DenotTransformers.{DenotTransformer, IdentityDenotTransformer, InfoTransformer}
import dotty.tools.dotc.transform.{Pickler, SetRootTree}



class PhaseSelectAndGenerateShiftedMethods(selectedNodes: SelectedNodes) extends PluginPhase with IdentityDenotTransformer  {

  val phaseName = PhaseSelectAndGenerateShiftedMethods.phaseName

  override val runsAfter = Set(SetRootTree.name)
  override val runsBefore = Set(Pickler.name, PhaseCps.name)

  override def changesMembers: Boolean = true
  override def changesParents: Boolean = true


 

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {

      lazy val cpsTransformedAnnot = Symbols.requiredClass("cps.plugin.annotation.CpsTransformed")

      if (tree.symbol.denot.is(Flags.Inline)) then
        tree
      else
        val topTree = tree
        val optKind = SelectedNodes.detectDefDefSelectKind(tree)
        optKind.foreach{kind =>
          tree.symbol.addAnnotation(cpsTransformedAnnot)
          selectedNodes.addDefDef(tree.symbol,kind)
        }
        // TODO:  try run this onlu on selected nodes
        val childTraversor = new TreeTraverser {
          override def traverse(tree: Tree)(using Context): Unit = {
            tree match
              case fun: DefDef if (fun.symbol != topTree.symbol) =>
                selectedNodes.getDefDefRecord(tree.symbol) match
                  case Some(r) =>
                    if (!r.internal) {
                      selectedNodes.markAsInternal(tree.symbol)
                      traverseChildren(tree)
                    }
                  case None =>
                    traverseChildren(tree)
              case Block(List(ddef:DefDef), closure:Closure) if ddef.symbol == closure.meth.symbol =>
                traverseChildren(tree)
              case Block(List(ddef:DefDef), Typed(closure:Closure, tp)) if ddef.symbol == closure.meth.symbol =>
                traverseChildren(tree)
              case Block(stats, expr) =>
                // don't mark local function definitions and templates as internal
                for(s <- stats) {
                  s match
                    case defDef: DefDef  =>
                        //traverse(defDef)
                    case tdef: TypeDef =>
                        // do nothing
                    case other =>
                        traverse(other)
                }
                traverse(expr)
              case _ =>
                traverseChildren(tree)
          }
        }
        childTraversor.traverse(tree)
        tree
  }

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    tree.rhs match
      case EmptyTree =>
        tree
      case other =>
        if (!tree.symbol.flags.isOneOf(Flags.InlineOrProxy|Flags.Synthetic) &&
            CpsTransformHelper.isCpsDirectType(tree.rhs.tpe)) then
              report.error("CpsDirect can't be a value", tree.srcPos)
              tree
        else
          super.transformValDef(tree)
  }

  override def transformAssign(tree: tpd.Assign)(using Context): tpd.Tree = {
    if (CpsTransformHelper.isCpsDirectType(tree.tpe)) then
      report.error("CpsDirect can't be a assigned", tree.srcPos)
      tree
    else
      super.transformAssign(tree)
  }

  // generate shifted version for hight-order functions annotated by makeCPS
  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    println(s"transformTemplate: ${tree.symbol}")
    val makeCpsAnnot = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    val shiftedMethods = tree.body.filter(_.symbol.annotations.exists(_.symbol == makeCpsAnnot))
      .flatMap { m =>
        m match
          case dd: DefDef => ShiftedMethodGenerator.generateShiftedMethod(dd)
          case other => 
            report.error("Only DefDef can be annotated by @makeCPS", other.srcPos)
            None
      }
    if (shiftedMethods.isEmpty) then
      tree
    else
      //despite this is not change signature,
      //tree.symbol.enteredAfter(this)
      //tree.symbol.asClass.enteredAfter(this)
      //tree
      for(m <- shiftedMethods) {
        m.symbol.enteredAfter(this)
      }
      cpy.Template(tree)(body = tree.body ++ shiftedMethods)
  }
  
  
}

object PhaseSelectAndGenerateShiftedMethods {

  val phaseName = "rssh.cpsSelect"

}