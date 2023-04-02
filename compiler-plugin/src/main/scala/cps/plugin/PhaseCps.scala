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
import dotty.tools.dotc.util.SrcPos

class PhaseCps(settings: CpsPluginSettings, selectedNodes: SelectedNodes, shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "rssh.cps"

  override def allowsImplicitSearch = true
  override val runsAfter = Set("rssh.cpsSelect")
  override val runsBefore = Set("rssh.cpsAsyncShift")

  val debug = true


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
    println(s"transform ${tree.symbol.showFullName}, (${tree.symbol.id}) starting at${tree.srcPos.startPos.show}, selectRecord.kind=${selectRecord.kind}")
    if (debugSettings.printCode) then
      report.log("transforming tree:", tree.srcPos)
    val retval = selectRecord.kind match
      case USING_CONTEXT_PARAM(cpsMonadContext) =>
        val tc = makeCpsTopLevelContext(cpsMonadContext,tree,Some(debugSettings))
        val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
        given CpsTopLevelContext = tc
        val transformedRhs = RootTransform(tree.rhs,tree.symbol,0).transformed
        val nRhs = Block(tc.cpsMonadValDef::Nil,transformedRhs)
        cpy.DefDef(tree)(tree.name, tree.paramss, TypeTree(nTpt), nRhs)
      case RETURN_CONTEXT_FUN(internalKind) =>
        tree.rhs match
          case Block((ddef: DefDef)::Nil, closure: Closure) =>
            val nDefDef = transformDefDefInternal(ddef, DefDefSelectRecord(kind=internalKind,internal=true))
            val nTpt = nDefDef.tpt
            //  if we not change defDef symbol,  call should be the same but with new type.
            val nClosure = cpy.Closure(closure)(closure.env, closure.meth, nTpt)
            val nLambda = cpy.Block(tree)(nDefDef::Nil,nClosure)
            cpy.DefDef(tree)(tree.name, tree.paramss,  nTpt, nLambda)
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
            val cpsMonadContext = ref(ddef.paramss(0)(0).symbol)
            val internalFun = transformDefDefInternal(ddef, DefDefSelectRecord(USING_CONTEXT_PARAM(cpsMonadContext),internal=true))
            val applyM = "applyM".toTermName
            val nApply = cpy.Apply(tree)(
              TypeApply(Select(inferAsyncArg, applyM), List(aCnd)),
              List(internalFun)
            )
            println(s"nApply=${nApply.show}")
            nApply
            /*
            infernAsyncArgCn.tpe match
              case AppliedType(tycon, tpargs) 
                if tycon.typeSymbol == Symbols.requiredClass("cps.CpsTransform.InfernAsyncArg") =>
                  val body = ddef.rhs
                  val bodyOwner = ddef.symbol
                  println(s"found CpsAsync candidate, tycon.typeSymbol=:  ${tycon.typeSymbol} ")
                  println(s"CpsAsync.body=:  ${body.show} ")
                  println(s"CpsAsync.bodyType=:  ${body.tpe.show} ")
                  println(s"funTree.symbol = ${ddef.symbol}, selectRecord=${selectedNodes.getDefDefRecord(ddef.symbol)}")
                  try
                    val params = ddef.paramss(0) match
                      case ValDefs(valDefs) => valDefs
                      case _ => throw CpsTransformException("CpsAsync lambda should be without type parameters", tree.srcPos)
                    val cpsMonadContext = ref(params(0).symbol)
                    val cpsTransformType = params(0).tpt.tpe
                    val monadType = CpsTransformHelper.extractMonadType(cpsTransformType,tree.srcPos)
                    val optRuntimeAwait = CpsTransformHelper.findRuntimeAwait(monadType, tree.span)
                    val mt = MethodType(List(params(0).name))(
                      x => List(params(0).tpt.tpe),
                      //x => tree.tpe.widen  //decorateTypeApplications(monadType).appliedTo(body.tpe)
                      x => decorateTypeApplications(monadType).appliedTo(body.tpe)
                    )
                    // TODO:  pass am to apply 
                    //val amInit = Select(infernAsyncArgCn,"am".toTermName)
                    val amInit = Select(cpsMonadContext, "monad".toTermName).withSpan(tree.span)
                    // TODO: changeOwnwe ?
                    val amValDef = SyntheticValDef("m2".toTermName,amInit)
                    val am = ref(amValDef.symbol)
                    val meth = Symbols.newAnonFun(summon[Context].owner,mt)
                    val ctxFun = Closure(meth, tss => {
                                          // here = check that valDef constructire chaned amOwner
                      val tc = CpsTopLevelContext(monadType, amValDef, am, params(0),optRuntimeAwait,DebugSettings.make(tree))
                      given CpsTopLevelContext = tc
                      println(s"cpsAsync:tc, debugLevel=${tc.settings.debugLevel}")
                      val cpsTree = RootTransform(body,bodyOwner, 0)
                      val transformedBody = Block(amValDef::Nil, cpsTree.transformed)
                      TransformUtil.substParams(transformedBody,List(params(0)),tss.head)
                                    .changeOwner(bodyOwner,meth)
                                    .withSpan(body.span)
                    }) 
                    val apply = Apply(
                                TypeApply(Select(am,"apply".toTermName),List(TypeTree(body.tpe.widen))),
                                List(ctxFun)
                    ).withSpan(tree.span)
                    val retval = Block(
                      List(amValDef),
                      apply
                    )
                    println(s"origin cpsAsync tree: ${tree.show}")
                    println(s"transformed cpsAwait tree: ${retval.show}")
                    println(s"ctxFun=${ctxFun.show}")

                    retval
                  catch
                    case ex:CpsTransformException =>
                      report.error(ex.message,ex.pos)
                      if (debug) {
                        ex.printStackTrace()
                      }
                      tree
              case _ =>
                  super.transformApply(tree)
            */
      case _ => super.transformApply(tree)

 
  }




  private def makeCpsTopLevelContext(cpsMonadContext: Tree, tree: Tree, optSettings:Option[DebugSettings]=None)(using Context): CpsTopLevelContext =  {
    val monadInit = Select(cpsMonadContext, "monad".toTermName).withSpan(tree.span)
    val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, tree.srcPos)
    val optRuntimeAwait = if(settings.useLoom) CpsTransformHelper.findRuntimeAwait(monadType, tree.span) else None
    val monadValDef = SyntheticValDef("m".toTermName, monadInit).changeOwner(summon[Context].owner, tree.symbol)
    val monadRef = ref(monadValDef.symbol)
    val debugSettings = optSettings.getOrElse(DebugSettings.make(tree))
    val tc = CpsTopLevelContext(monadType, monadValDef, monadRef, cpsMonadContext, optRuntimeAwait, debugSettings)
    tc
  }


}

