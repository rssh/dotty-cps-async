package cps.plugin

import scala.annotation.*
import dotty.tools.dotc.*
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import cps.plugin.DefDefSelectKind.{RETURN_CONTEXT_FUN, USING_CONTEXT_PARAM}
import plugins.*
import cps.plugin.QuoteLikeAPI.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.DenotTransformers.{InfoTransformer, SymTransformer}
import dotty.tools.dotc.util.SrcPos


/**
 * Phase where we do cps transformation. Note, that this phase should run before inlining, because when we
 *  search for async-shift object, it can be inlined.
 * @param settings
 * @param selectedNodes
 * @param shiftedSymbols
 */
class PhaseCps(settings: CpsPluginSettings, selectedNodes: SelectedNodes, shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = PhaseCps.name

  override def allowsImplicitSearch = true

  override def changesMembers: Boolean = true


  override val runsAfter = Set("rssh.cpsSelect")
  override val runsBefore = Set("rssh.cpsAsyncShift", PhaseCpsChangeSymbols.name)


  val debug = true


  override def prepareForDefDef(tree: tpd.DefDef)(using Context): Context = {
    super.prepareForDefDef(tree).withPhase(this)
  }



  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    selectedNodes.getDefDefRecord(tree.symbol) match
      case Some(selectRecord) if (!selectRecord.internal) =>
          try
            transformDefDefInternal(tree, selectRecord, None)
          catch
            case ex: CpsTransformException =>
              report.error(ex.message, ex.pos)
              //ex.printStackTrace()
              throw ex
      case _ => tree
  }

  def transformDefDefInternal(tree: DefDef, selectRecord: DefDefSelectRecord, optTopLevelContext:Option[CpsTopLevelContext]=None)(using Context): DefDef = {
    val debugSettings = optTopLevelContext.map(_.settings).getOrElse(DebugSettings.make(tree))
    println(s"transformDefDef ${tree.symbol.showFullName}, (${tree.symbol.id}) starting at${tree.srcPos.startPos.show}, selectRecord.kind=${selectRecord.kind}")
    if (debugSettings.printCode) then
      report.log("transforming tree:", tree.srcPos)
    val retval = selectRecord.kind match
      case USING_CONTEXT_PARAM(cpsMonadContextArg) =>
        val cpsMonadContext = ref(cpsMonadContextArg.symbol)
        val tc = makeCpsTopLevelContext(cpsMonadContext,tree,Some(debugSettings))
        val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
        selectRecord.monadType = tc.monadType
        //selectRecord.changedReturnType = nTpt
        given CpsTopLevelContext = tc
        val ctx1: Context = summon[Context].withOwner(tree.symbol)
        val transformedRhs = RootTransform(tree.rhs,tree.symbol,0)(using ctx1, tc).transformed
        val nRhs = Block(tc.cpsMonadValDef::Nil,transformedRhs)(using ctx1)
        println(s"nRsh.block=${nRhs.show}")
        println(s"nRhs.tpe = ${nRhs.tpe.show}")
        val adoptedRhs = Scaffolding.adoptUncpsedRhs(nRhs, tree.tpt.tpe, tc.monadType)
        val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, adoptedRhs)
        println(s"transformDefDef[1] for ${tree.symbol.id}  ")
          //tree.symbol.defTree = retval
        retval
      case RETURN_CONTEXT_FUN(internalKind) =>
        tree.rhs match
          case oldLambda@Block((ddef: DefDef)::Nil, closure: Closure) =>
            val nDefDef = transformDefDefInternal(ddef, DefDefSelectRecord(kind=internalKind,internal=true))
            val cpsMonadContext = ref(selectRecord.kind.getCpsMonadContext.symbol)
            val fType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe, tree.srcPos)
            selectRecord.monadType = fType
            val nClosureType = CpsTransformHelper.cpsTransformedType(closure.tpe, fType)
            //selectRecord.changedReturnType = nClosureType
            val nClosure = Closure(closure.env, ref(nDefDef.symbol), EmptyTree).withSpan(closure.span)
            println(s"creating closure (ref to ${nDefDef.symbol.id}):${nClosure} ")
            println(s"closure.tpe=${closure.tpe.show},  nClosure.tpe=${nClosure.tpe.show}")
            val nLambda = Block(nDefDef::Nil,nClosure).withSpan(oldLambda.span)
            println(s"Block.tpe = ${nLambda.tpe.show}")
            val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, nLambda)
            retval
          case _ =>
            throw CpsTransformException("Lambda function was expected, we have $tree",tree.srcPos)
    if (debugSettings.printCode) then
      report.log(s"transforned: ${retval.show}",tree.srcPos)
      report.log(s"transforned: ${retval}",tree.srcPos)
    retval
  }


  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    try
      transformApplyInternal(tree)
    catch
      case ex: CpsTransformException =>
        report.error(ex.message,ex.pos)
        throw ex;
  }

  def transformApplyInternal(tree: Apply)(using Context):Tree = {
    tree match
      case Apply(
            TypeApply(Select(inferAsyncArg,applyCn),List(aCnd)),
            List(ctxFun@Block((ddef: DefDef)::Nil, closure: Closure))
         )  if ddef.symbol == closure.meth.symbol &&
              inferAsyncArg.tpe.typeSymbol == Symbols.requiredClass("cps.CpsTransform.InfernAsyncArg")  &&
              applyCn.mangledString == "apply"
           =>
            println(s"found CpsAsync candidate: ${tree.show} ")
            println(s"CpsAsync.ctx=:  ${ddef.show} ")
            println(s"inferAsyncArg=:  ${inferAsyncArg} ")
            val cpsMonadContext = ref(ddef.paramss(0)(0).symbol)
            val internalFun = transformDefDefInternal(ddef, DefDefSelectRecord(USING_CONTEXT_PARAM(cpsMonadContext),internal=true))
            println(s"closure.tpt=${closure.tpt}")
            //val internalClosure = cpy.Closure(closure)(closure.env, internalFun, closure.tpt)
            val internalClosure = Closure(closure.env, ref(internalFun.symbol), EmptyTree).withSpan(closure.span)
            val applyMArg = Block(internalFun::Nil, internalClosure).withSpan(ctxFun.span)
            println(s"internalFun=${internalFun}")
            println(s"internalFun.tpt=${internalFun.tpt}")
            println(s"internalFun.tpt.tpe=${internalFun.tpt.tpe}")
            val applyM = "applyM1".toTermName
            val nApply = cpy.Apply(tree)(
              TypeApply(Select(inferAsyncArg, applyM), List(aCnd)),
              List(applyMArg)
            )
            println(s"nApply=${nApply.show}")
            nApply
      case _ => super.transformApply(tree)

 
  }




  private def makeCpsTopLevelContext(cpsMonadContext: Tree, tree: Tree, optSettings:Option[DebugSettings]=None)(using Context): CpsTopLevelContext =  {
    println(s"mekeCpsTopLevelContext: cpsMonadContext=${cpsMonadContext}")
    cpsMonadContext match
      case vd: ValDef =>
        println("!! - found incorrect makeCpsTopLevelContext")
        throw CpsTransformException("incorrect makeCpsTopLevelContext", tree.srcPos)
      case _ =>
    val monadInit = Select(cpsMonadContext, "monad".toTermName).withSpan(tree.span)
    val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, tree.srcPos)
    val optRuntimeAwait = if(settings.useLoom) CpsTransformHelper.findRuntimeAwait(monadType, tree.span) else None
    val monadValDef = SyntheticValDef("m".toTermName, monadInit)(using summon[Context].fresh.setOwner(tree.symbol))
    println(s"monadValDef=${monadValDef.show}")
    //val monadValDef = monadValDef0.changeOwner(monadValDef0.symbol.owner, tree.symbol)
    //println(s"!! monadVaDef0.owner.id=${monadValDef0.symbol.owner.id}, monadValDef.owner.id=${monadValDef.symbol.owner.id}, tree.symbol.id=${tree.symbol.id}")
    val monadRef = ref(monadValDef.symbol)
    val debugSettings = optSettings.getOrElse(DebugSettings.make(tree))
    val tc = CpsTopLevelContext(monadType, monadValDef, monadRef, cpsMonadContext, optRuntimeAwait, debugSettings)
    tc
  }


}

object PhaseCps {

  def name: String = "rssh.cps"

}
