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
import cps.plugin.observatory.AutomaticColoringAnalyzer
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.DenotTransformers.{InfoTransformer, SymTransformer}
import dotty.tools.dotc.util.SrcPos
import transform.{ElimPackagePrefixes, Erasure, Inlining, Pickler}


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


  override val runsAfter = Set("rssh.cpsSelect", Inlining.name, Pickler.name)
  override val runsBefore = Set("rssh.cpsAsyncShift", ElimPackagePrefixes.name, Erasure.name, PhaseCpsChangeSymbols.name)


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
    val debugSettings = optTopLevelContext.map(_.debugSettings).getOrElse(DebugSettings.make(tree))
    selectRecord.debugLevel = debugSettings.debugLevel
    println(s"transformDefDef ${tree.symbol.showFullName}, (${tree.symbol.id}) starting at${tree.srcPos.startPos.show}, selectRecord.kind=${selectRecord.kind}")
    if (debugSettings.printCode) then
      report.log("transforming tree:", tree.srcPos)
    val retval = selectRecord.kind match
      case USING_CONTEXT_PARAM(cpsMonadContextArg) =>
        val cpsMonadContext = ref(cpsMonadContextArg.symbol)
        val tc = makeCpsTopLevelContext(cpsMonadContext,tree.symbol,tree.srcPos,debugSettings, CpsTransformHelper.cpsDirectClassSymbol)
        val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
        selectRecord.monadType = tc.monadType
        //selectRecord.changedReturnType = nTpt
        given CpsTopLevelContext = tc
        val ctx1: Context = summon[Context].withOwner(tree.symbol)
        val transformedRhs = RootTransform(tree.rhs,tree.symbol, 0)(using ctx1, tc).transformed
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
            val cpsDirectContext = ref(selectRecord.kind.getCpsDirectContext.symbol)
            val fType = CpsTransformHelper.extractMonadType(cpsDirectContext.tpe, CpsTransformHelper.cpsDirectClassSymbol, tree.srcPos)
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

            val nDefDef = transformDefDefInsideAsync(ddef, tree, true)
            val nClosure = Closure(closure.env, ref(nDefDef.symbol), EmptyTree).withSpan(closure.span)

            val applyMArg = Block(nDefDef::Nil, nClosure).withSpan(ctxFun.span)
            val applyM = "applyM1".toTermName
            val nApply = cpy.Apply(tree)(
              TypeApply(Select(inferAsyncArg, applyM), List(aCnd)),
              List(applyMArg)
            )
            nApply
      case Apply(
            TypeApply( cpsAsyncApplyCn, List(fType, tType, cType) ),
            List( am, ctxFun )
           )  if (cpsAsyncApplyCn.symbol == Symbols.requiredMethod("cps.plugin.cpsAsyncApply")) =>
        val (ddef, closure) = ctxFun match
          case Inlined(call, List(), Block((ddef: DefDef)::Nil, closure: Closure)) =>
            println(s"transformApplyInternal: Inlined found ddef.symbol=${ddef.symbol.showFullName}")
            (ddef, closure)
          case Block((ddef: DefDef)::Nil, closure: Closure) =>
            println(s"transform Apply: found ddef.symbol=${ddef.symbol.showFullName}")
            (ddef, closure)
          case _ =>
            throw CpsTransformException(s"excepted that second argument of cpsAsyncApply is closure, we have $ctxFun", tree.srcPos)
        val nDefDef = transformDefDefInsideAsync(ddef, tree, false)
        if (ddef.symbol.owner != ctx.owner) {
          println(s"transformApplyInternal: ddef.symbol.owner != ctx.owner  ddef.symbol.owner=${ddef.symbol.owner.showFullName}, ctx.owner=${ctx.owner.showFullName}")
        }
        if (ddef.symbol.hashCode() == 41394) {
          println(s"transformApplyInternal: 41394 found: ddef.symbol=${ddef.symbol.showFullName}")
          println(s"nDedDef.symbol = {nDefDef.symbol} (${nDefDef.symbol.hashCode()})")
        }
        val nClosure = Closure(closure.env, ref(nDefDef.symbol), EmptyTree).withSpan(closure.span)
        val applyArg = Block(nDefDef::Nil, nClosure).withSpan(ctxFun.span)
        val nApply = cpy.Apply(tree)(
          TypeApply(Select(am, "apply".toTermName), List(tType)),
          List(applyArg)
        )
        nApply
      case Apply(
             TypeApply( cpsAsyncStreamApplyCn, List(rType, fType, tType, cType)  ),
             List(absorber, ctxFun)
           ) if (cpsAsyncStreamApplyCn.symbol == Symbols.requiredMethod("cps.plugin.cpsAsyncStreamApply")) =>
        transformCpsAsyncStreamApply(tree,absorber,ctxFun)
      case _ => super.transformApply(tree)

 
  }




  private def transformDefDefInsideAsync(ddef: DefDef, asyncCallTree: Tree, contextual: Boolean)(using ctx:Context): DefDef = {
    val cpsMonadContext = ddef.paramss.head.head
    val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, CpsTransformHelper.cpsMonadContextClassSymbol, asyncCallTree.srcPos)

    //val nRhsType = CpsTransformHelper.cpsTransformedType(ddef.rhs.tpe.widen, monadType)
    //val mt = if (contextual) {
    //  ContextualMethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.tpe.widen), _ => nRhsType)
    //} else {
    //  MethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.tpe.widen), _ => nRhsType)
    //}
    //val newSym = Symbols.newAnonFun(ctx.owner, mt, ddef.span)



    if (true) {
      println(s"transformDefDefInsideAsync: checking correct input ctx.owner=${ctx.owner.hashCode()}")
      TransformUtil.findSubtermsWithIncorrectOwner(ddef.rhs, ddef.symbol) match
        case subterm::tail =>
          println(s"found subterm with incorrect owner: ${subterm.symbol}(${subterm.symbol.hashCode()}  owner=${subterm.symbol.owner}(${subterm.symbol.owner.hashCode()})")
        case Nil =>
          println(s"not found subterm with incorrect owner")
    }

    val contextParam = cpsMonadContext match
      case vd: ValDef => ref(vd.symbol)
      case _ => throw CpsTransformException(s"excepted that cpsMonadContext is ValDef, but we have ${cpsMonadContext.show}", asyncCallTree.srcPos)
    val tctx = makeCpsTopLevelContext(contextParam, ddef.symbol, asyncCallTree.srcPos, DebugSettings.make(asyncCallTree), CpsTransformHelper.cpsMonadContextClassSymbol)
    val ddefCtx = ctx.withOwner(ddef.symbol)
    tctx.automaticColoring.foreach(_.analyzer.observe(ddef.rhs)(using ddefCtx))
    val nRhsCps = RootTransform(ddef.rhs, ddef.symbol, 0)(using ddefCtx, tctx)
    val nRhsTerm = wrapTopLevelCpsTree(nRhsCps)(using ddefCtx, tctx)
    val nRhsType = nRhsTerm.tpe.widen
    val mt = if (contextual) {
      ContextualMethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.tpe.widen), _ => nRhsType)
    } else {
      MethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.tpe.widen), _ => nRhsType)
    }
    val newSym = Symbols.newAnonFun(ctx.owner, mt, ddef.span)
    val nDefDef = DefDef(newSym, paramss => {
      implicit val nctx = ctx.withOwner(newSym)
      val monadValDef = tctx.cpsMonadValDef
      val body = Block(monadValDef::Nil, nRhsTerm).withSpan(ddef.rhs.span)
      val nBody = TransformUtil.substParams(body, ddef.paramss.head.asInstanceOf[List[ValDef]], paramss.head)
        .changeOwner(ddef.symbol, newSym)
        //  note, that changeOwner should be last, because otherwise substParams will not see origin params.
      nBody
    }).withSpan(ddef.span)
    println(s"transformDefDefInsideAsync: newSym=${newSym.hashCode()}, old ddef.sym=${ddef.symbol.hashCode()}, ctx.owner=${ctx.owner.hashCode()}")
    println(s"old defdef type: ${ddef.tpe.widen.show}")
    println(s"new defdef type: ${nDefDef.tpe.widen.show}")

    if (true) {
      TransformUtil.findSubtermWithOwner(nDefDef.rhs, ddef.symbol) match
        case Some(tree) => println(s"err::symbol still have old owner:  ${tree.show}")
        case None =>
    }

    /*
    val nDefDef = DefDef(newSym, paramss => {
      given tctx: CpsTopLevelContext = makeCpsTopLevelContext(paramss.head.head, newSym, asyncCallTree.srcPos, DebugSettings.make(asyncCallTree), CpsTransformHelper.cpsMonadContextClassSymbol)
      val nctx = ctx.withOwner(newSym)
      TransformUtil.findSubtermWithOtherOwner(ddef.rhs, ddef.symbol) match
        case Some(tree) => println(s"err::symbol have other owner then ddef:  ${tree.show} with owner ${tree.symbol.owner.hashCode()}")
        case None =>
      val nBody = TransformUtil.substParams(ddef.rhs, ddef.paramss.head.asInstanceOf[List[ValDef]], paramss.head).changeOwner(ddef.symbol, newSym)
      TransformUtil.findSubtermWithOwner(nBody, ddef.symbol) match
        case Some(tree) => println(s"err::symbol still have old owner:  ${tree.show}")
        case None =>
      val nRhsCps = RootTransform(nBody, newSym, 0)(using nctx, tctx)
      val nRhs = wrapTopLevelCpsTree(nRhsCps)(using nctx, tctx)
      Block(tctx.cpsMonadValDef::Nil, nRhs)(using nctx)
    } ).withSpan(ddef.span)

     */
    nDefDef
  }


  /**
   * '''
   * *  cpsAsyncStreamApply[R,F[_],T,C](absorber, { CtxContext ?=> (Emitter => (body: Unit)) })
   * '''
   *
   * will be transformed to:
   *
   * ```
   *  ${absorber}.eval(CtxContext => (Emitter => cpsTransform(body):F[Unit]))
   * ```
   * */
  private def transformCpsAsyncStreamApply(origin: Apply, absorber: Tree, funfun: Tree)(using ctx:Context): Tree = {
     Apply(
       Select(absorber, "eval".toTermName),
       List(
          transformDefDef1InsideCpsAsyncStreamApply(funfun, ctx.owner)
       )
     ).withSpan(origin.span)
  }

  private def transformDefDef1InsideCpsAsyncStreamApply(funfun: Tree, owner: Symbol)(using ctx: Context): Tree = {
    funfun match
      case Inlined(call, env, body) =>
        if (env.isEmpty) {
          transformDefDef1InsideCpsAsyncStreamApply(body, owner)
        } else {
          Inlined(call, env, transformDefDef1InsideCpsAsyncStreamApply(body, owner))
        }
      case Block((ddef: DefDef)::Nil, closure: Closure) =>
        val ctxValDef = ddef.paramss.head.head
        val ctxRef = ref(ctxValDef.symbol)
        val nInternalLambda = transformDefDef2InsideCpsAsyncStreamApply(ddef.rhs, ctxRef)(using ctx.withOwner(ddef.symbol))
        val mt = MethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.typeOpt.widen), _ => nInternalLambda.tpe.widen)
        val newSym = Symbols.newAnonFun(owner, mt, ddef.span)
        val nDefDef = DefDef(newSym, { paramss =>
          val retval = TransformUtil.substParams(nInternalLambda,ddef.paramss.head.asInstanceOf[List[ValDef]],paramss.head).changeOwner(ddef.symbol, newSym)
          retval
        })
        val nClosure = Closure(closure.env, ref(newSym), EmptyTree).withSpan(closure.span)
        val nBlock = Block(nDefDef::Nil, nClosure).withSpan(funfun.span)
        nBlock
  }

  private def transformDefDef2InsideCpsAsyncStreamApply(fun:Tree, ctxRef: Tree)(using ctx: Context): Tree = {
    fun match
      case Inlined(call, env, body) =>
        Inlined(call, env, transformDefDef2InsideCpsAsyncStreamApply(body, ctxRef))
      case Block((ddef: DefDef)::Nil, closure: Closure) if (ddef.symbol == closure.meth.symbol) =>
        //val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, CpsTransformHelper.cpsMonadContextClassSymbol, asyncCallTree.srcPos)
        val tctx = makeCpsTopLevelContext(ctxRef, ddef.symbol, ddef.rhs.srcPos, DebugSettings.make(ddef), CpsTransformHelper.cpsMonadContextClassSymbol)
        val ddefContext = ctx.withOwner(ddef.symbol)
        val nRhsCps = RootTransform(ddef.rhs, ddef.symbol, 0)(using ddefContext, tctx)
        val nRhs = Block(tctx.cpsMonadValDef::Nil, nRhsCps.transformed(using ddefContext, tctx))
        val mt = MethodType(ddef.paramss.head.map(_.name.toTermName))(_ => ddef.paramss.head.map(_.typeOpt.widen), _ => nRhs.tpe.widen)
        val newSym = Symbols.newAnonFun(ctx.owner, mt, ddef.span)
        val nDefDef = DefDef(newSym, { paramss =>
            val retval = TransformUtil.substParams(nRhs, ddef.paramss.head.asInstanceOf[List[ValDef]], paramss.head).changeOwner(ddef.symbol, newSym)
            retval
        })
        val nClosure = Closure(closure.env, ref(newSym), EmptyTree).withSpan(closure.span)
        val nBlock = Block(nDefDef::Nil, nClosure).withSpan(fun.span)
        nBlock
  }


  def wrapTopLevelCpsTree(cpsTree: CpsTree)(using Context, CpsTopLevelContext): Tree = {
    val tctx = summon[CpsTopLevelContext]

    def makeSync(unpure: Tree): Tree = {
      Apply(
        TypeApply(
          Select(tctx.cpsMonadRef, "wrap".toTermName),
          List(TypeTree(cpsTree.originType.widen))
        ),
        List(unpure)
      ).withSpan(cpsTree.origin.span)
    }

    def makeAsync(transformed: Tree) = {
      Apply(
        TypeApply(
          Select(tctx.cpsMonadRef, "flatWrap".toTermName),
          List(TypeTree(cpsTree.originType.widen))
        ),
        List(transformed)
      ).withSpan(cpsTree.origin.span)
    }

    /**
     * Just for correct diagnosting, aboput applying normalization
     * (which we want to eliminate to normalize 'on fly')
     * @param cpsTree
     * @return
     */
    def tryNormalize(cpsTree: CpsTree): CpsTree = {
      cpsTree.asyncKind match
        case AsyncKind.Sync => cpsTree
        case AsyncKind.Async(AsyncKind.Sync) => cpsTree
        case AsyncKind.AsyncLambda(bodyKind) =>
          throw CpsTransformException(s"unsupported lambda in top level wrap: ${cpsTree.show}", cpsTree.origin.srcPos)
        case _ =>
          report.warning(s"Unnormalized cpsTree: ${cpsTree.show}", cpsTree.origin.srcPos)
          cpsTree.normalizeAsyncKind
    }

    cpsTree.unpure match
      case Some(unpure) => makeSync(unpure)
      case None =>
        tryNormalize(cpsTree).asyncKind match
          case AsyncKind.Async(AsyncKind.Sync) =>
            makeAsync(cpsTree.transformed)
          case _ =>
            println(s"CpsTree with non-standard async kind: ${cpsTree.show}")
            throw CpsTransformException(s"unsupported type for top-level wrap, asyncKind=${cpsTree.asyncKind}", cpsTree.origin.srcPos)
  }


  /**
   * create cps top-level context for transformation.
   * @param cpsDirectOrSimpleContext - reference to cpsMonadContext.
   * @param srcPos - position whre show monadInit
   **/
  private def makeCpsTopLevelContext(cpsDirectOrSimpleContext: Tree, owner:Symbol, srcPos: SrcPos, debugSettings:DebugSettings, wrapperSym: ClassSymbol)(using Context): CpsTopLevelContext =  {
    cpsDirectOrSimpleContext match
      case vd: ValDef =>
        throw CpsTransformException("incorrect makeCpsTopLevelContext", srcPos)
      case _ =>
    val monadInit = Select(cpsDirectOrSimpleContext, "monad".toTermName).withSpan(srcPos.span)
    val monadType = CpsTransformHelper.extractMonadType(cpsDirectOrSimpleContext.tpe.widen, wrapperSym, srcPos)
    val optRuntimeAwait = if(settings.useLoom) CpsTransformHelper.findRuntimeAwait(monadType, srcPos.span) else None
    val monadValDef = SyntheticValDef("m".toTermName, monadInit)(using summon[Context].fresh.setOwner(owner))
    val monadRef = ref(monadValDef.symbol)
    val optThrowSupport = CpsTransformHelper.findCpsThrowSupport(monadType, srcPos.span)
    val optTrySupport = CpsTransformHelper.findCpsTrySupport(monadType, srcPos.span)
    val isBeforeInliner = if (runsBefore.contains(Inlining.name)) { true }
                          else if (runsAfter.contains(Inlining.name)) { false }
                          else
                             throw new CpsTransformException("plugins runsBefore/After Inlining not found", srcPos)
    val automaticColoringTag = CpsTransformHelper.findAutomaticColoringTag(monadType, srcPos.span)
    val automaticColoring = if (automaticColoringTag.isDefined) {
      println("found automatic coloring")
      val memoization = CpsTransformHelper.findCpsMonadMemoization(monadType, srcPos.span)
      if (memoization.isDefined) {
        val analyzer = new AutomaticColoringAnalyzer()
        Some(CpsAutomaticColoring(memoization.get,analyzer))
      } else {
        throw CpsTransformException(s"Can't find instance of cps.CpsMemoization for ${monadType.show}", srcPos)
      }
    } else None
    val tc = CpsTopLevelContext(monadType, monadValDef, monadRef, cpsDirectOrSimpleContext,
                                optRuntimeAwait, optThrowSupport, optTrySupport,
                                debugSettings, settings, isBeforeInliner, automaticColoring)
    tc
  }


}

object PhaseCps {

  def name: String = "rssh.cps"

}
