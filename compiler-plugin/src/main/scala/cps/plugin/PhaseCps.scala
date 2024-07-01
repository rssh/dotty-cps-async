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
import transform.{ElimPackagePrefixes, Erasure, Inlining, Pickler}


/**
 * Phase where we do cps transformation. Note, that this phase should run before inlining, because when we
 *  search for async-shift object, it can be inlined.
 * @param settings
 * @param selectedNodes
 */
class PhaseCps(settings: CpsPluginSettings,
               selectedNodes: SelectedNodes) extends PluginPhase {

  val phaseName = PhaseCps.name

  override def allowsImplicitSearch = true

  override def changesBaseTypes: Boolean = true
  override def changesMembers: Boolean = true


  override val runsAfter = Set(PhaseSelectAndGenerateShiftedMethods.phaseName, Inlining.name, Pickler.name)
  override val runsBefore = Set(PhaseChangeSymbolsAndRemoveScaffolding.name, ElimPackagePrefixes.name, Erasure.name)


  val debug = true


  override def prepareForDefDef(tree: tpd.DefDef)(using Context): Context = {
    super.prepareForDefDef(tree).withPhase(this)
  }



  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    selectedNodes.getDefDefRecord(tree.symbol) match
      case Some(selectRecord) if (!selectRecord.internal)  =>
          try
              transformDefDefInternal(tree, selectRecord, None)
          catch
            case ex: CpsTransformException =>
              report.error(ex.message, ex.pos)
              //ex.printStackTrace()
              throw ex
      case _ =>
        tree
  }


  def transformDefDefInternal(tree: DefDef, selectRecord: DefDefSelectRecord, optTopLevelContext:Option[CpsTopLevelContext]=None)(using Context): DefDef = {
    val debugSettings = optTopLevelContext.map(_.debugSettings).getOrElse(DebugSettings.make(tree, settings))
    selectRecord.debugLevel = debugSettings.debugLevel
    val retval = selectRecord.kind match
      case USING_CONTEXT_PARAM(cpsMonadContextArg) =>
        val cpsMonadContext = ref(cpsMonadContextArg.symbol)
        if (tree.rhs.isEmpty) then
          selectRecord.monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen,CpsTransformHelper.cpsDirectAliasSymbol,tree.srcPos)
          tree
        else
          val (tc, monadValDef) = makeCpsTopLevelContext(cpsMonadContext,tree.symbol,tree.srcPos,debugSettings, CpsTransformHelper.cpsDirectAliasSymbol)
          val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
          selectRecord.monadType = tc.monadType
          //selectRecord.changedReturnType = nTpt
          given CpsTopLevelContext = tc
          val ctx1: Context = summon[Context].withOwner(tree.symbol)
          if (debugSettings.printCode) then
            Log.info(s"transformDefDefIntenal: ${tree.show}",0, tree.srcPos)
          val transformedRhs = RootTransform(tree.rhs,tree.symbol, 0)(using ctx1, tc).transformed
          val nRhs = Block(monadValDef::Nil,transformedRhs)(using ctx1)
          val adoptedRhs = Scaffolding.adoptUncpsedRhs(nRhs, tree.tpt.tpe, tc.monadType)
          val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, adoptedRhs)
          if (debugSettings.printCode) then
            Log.info(s"transformDefDefInternal: transformed: ${retval.show}",0, tree.srcPos)
          retval
      case RETURN_CONTEXT_FUN(internalKind) =>
        val cpsDirectContext = ref(selectRecord.kind.getCpsDirectContext.symbol)
        val fType = CpsTransformHelper.extractMonadType(cpsDirectContext.tpe, CpsTransformHelper.cpsDirectAliasSymbol, tree.srcPos)
        selectRecord.monadType = fType
        tree.rhs match
          case oldLambda@Block((ddef: DefDef)::Nil, closure: Closure) =>
            val nDefDef = transformDefDefInternal(ddef, DefDefSelectRecord(kind=internalKind,internal=true))
            val nClosureType = CpsTransformHelper.cpsTransformedType(closure.tpe, fType)
            //selectRecord.changedReturnType = nClosureType
            val nClosure = Closure(closure.env, ref(nDefDef.symbol), EmptyTree).withSpan(closure.span)
            val nLambda = Block(nDefDef::Nil,nClosure).withSpan(oldLambda.span)
            val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, nLambda)
            retval
          case EmptyTree =>
            tree
          case _ =>
            throw CpsTransformException("Lambda function was expected, we have $tree",tree.srcPos)
    retval
  }

  override def prepareForApply(tree: tpd.Apply)(using Context): Context = {
    if (summon[Context].phase != this) {
      println(s"PhaseCps::prepareForApply, invalid phase = ${summon[Context].phase}")
      summon[Context].withPhase(this)
    } else {
      summon[Context]
    }
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
            (ddef, closure)
          case Block((ddef: DefDef)::Nil, closure: Closure) =>
            (ddef, closure)
          case _ =>
            throw CpsTransformException(s"excepted that second argument of cpsAsyncApply is closure, we have $ctxFun", tree.srcPos)
        val nDefDef = transformDefDefInsideAsync(ddef, tree, false)
        if (ddef.symbol.owner != ctx.owner) {
          report.warning(s"transformApplyInternal: ddef.symbol.owner != ctx.owner  ddef.symbol.owner=${ddef.symbol.owner.showFullName}, ctx.owner=${ctx.owner.showFullName}", tree.srcPos)
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
      case app@Apply(tapp@TypeApply(adoptCpsedCallCompileTimeOnlyCn, List(tf, ta)), List(a))
        if (adoptCpsedCallCompileTimeOnlyCn.symbol == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCallCompileTimeOnly")) =>
           val adoptCpsedCall = ref(Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")).withSpan(adoptCpsedCallCompileTimeOnlyCn.span)
           val nApp = Apply(TypeApply(adoptCpsedCall, List(tf, ta)), List(a)).withSpan(app.span)
           transformApplyInternal(nApp)
      case Apply(TypeApply(adoptCpsedCallCn, List(tf, ta)), List(a))
        if (adoptCpsedCallCn.symbol == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")) =>
          a match
            case ai@Inlined(call, bindings, expansion) =>
              // this is inline extension method defined inside asycn block which was transformed by macros.
              // Since extension methods are inlined after macros, macros have not seen it [dotty error ?], but
              // compiler inlined one and insert before PhaseCps call.
              //
              // in such case direct context is an argument of inlined call.
              //TODO: check Inline(Inline(...))
              findCpsDirectArg(call) match
                case Some(tree) =>
                  val (tc, monadValDef) = tree match
                    case CpsDirectHelper.ByInclusionCall(tf, tg, fctx, fginc) =>
                      makeCpsTopLevelContext(fctx, summon[Context].owner, a.srcPos, DebugSettings.make(a, settings), CpsTransformHelper.cpsMonadContextClassSymbol)
                    case CpsDirectHelper.NewCall(fctx) =>
                      makeCpsTopLevelContext(fctx, summon[Context].owner, a.srcPos, DebugSettings.make(a, settings), CpsTransformHelper.cpsMonadContextClassSymbol)
                    case other =>
                      makeCpsTopLevelContext(other, summon[Context].owner, a.srcPos, DebugSettings.make(a, settings), CpsTransformHelper.cpsDirectAliasSymbol)
                  val nTree = {
                    given CpsTopLevelContext = tc
                    RootTransform(a, summon[Context].owner, 0).transformed
                  }
                  // can strip adoptCpsedCall now.
                  Block(monadValDef::Nil, nTree).withSpan(a.span)
                case None =>
                  // impossible bevause afopedCpsedCall is inserted by async macro arround apply with CpsDirect arg.
                  report.warning(s"Unexpected argumnet of cps.plugin.scaffolding.adoptCpsedCall (should be apply) : ${a.show}", a.srcPos)
                  super.transformApply(tree)
            case _ =>
              super.transformApply(tree)
      case Apply(Apply(TypeApply(deferredAsyncCn, List(tp,mtp,mctp)), List(applyTerm)), List(ctx))
        if (deferredAsyncCn.symbol == Symbols.requiredMethod("cps.plugin.scaffolding.deferredAsync")) =>
            val (tc, monadValDef) = makeCpsTopLevelContext(ctx,summon[Context].owner, tree.srcPos,
                                      DebugSettings.make(tree, settings), CpsTransformHelper.cpsMonadContextClassSymbol)
            val nApplyTerm = {
              given CpsTopLevelContext = tc
              RootTransform(applyTerm, summon[Context].owner, 0).transformed
            }
            Block(monadValDef::Nil, nApplyTerm).withSpan(applyTerm.span)
      case _ => super.transformApply(tree)

 
  }




  private def transformDefDefInsideAsync(ddef: DefDef, asyncCallTree: Tree, contextual: Boolean)(using ctx:Context): DefDef = {
    val cpsMonadContext = ddef.paramss.head.head
    val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, CpsTransformHelper.cpsMonadContextClassSymbol, asyncCallTree.srcPos)

    if (true) {
      TransformUtil.findFirstSubtermWithIncorrectOwner(ddef.rhs, ddef.symbol) match
        case Some(TransformUtil.IncorrectOwnerRecord(contextOnwer, expectedOwner, realOwner, tree)) =>
          report.debugwarn(s"found subterm with incorrect owner: ${tree.symbol}(${tree.symbol.hashCode()}  owner=${realOwner}(${realOwner.hashCode()})", ddef.srcPos)
        case None =>
    }

    val contextParam = cpsMonadContext match
      case vd: ValDef => ref(vd.symbol)
      case _ => throw CpsTransformException(s"excepted that cpsMonadContext is ValDef, but we have ${cpsMonadContext.show}", asyncCallTree.srcPos)
    val (tctx, monadValDef) = makeCpsTopLevelContext(contextParam, ddef.symbol, asyncCallTree.srcPos,
                               DebugSettings.make(ddef, settings), CpsTransformHelper.cpsMonadContextClassSymbol)
    if (tctx.debugSettings.printCode) {
      Log.info(s"transformDefDefInsideAsync: ${ddef.show}", 0,  ddef.srcPos)(using ctx, tctx)
      Log.info(s"transformDefDefInsideAsync: body: ${ddef.rhs.show}", 0,  ddef.srcPos)(using ctx, tctx)
    }
    val ddefCtx = ctx.withOwner(ddef.symbol)
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
      val body = Block(monadValDef::Nil, nRhsTerm).withSpan(ddef.rhs.span)
      val nBody = TransformUtil.substParams(body, ddef.paramss.head.asInstanceOf[List[ValDef]], paramss.head)
        .changeOwner(ddef.symbol, newSym)
        //  note, that changeOwner should be last, because otherwise substParams will not see origin params.
      nBody
    }).withSpan(ddef.span)

    //if (true) {
    //  TransformUtil.findSubtermWithOwner(nDefDef.rhs, ddef.symbol) match
    //    case Some(tree) => println(s"err::symbol still have old owner:  ${tree.show}")
    //    case None =>
    //}
    if (tctx.debugSettings.printCode) {
      Log.info(s"transformDefDefInsideAsync: transformed: ${nDefDef.show}", 0, ddef.srcPos)(using ctx, tctx)
      Log.info(s"transformDefDefInsideAsync: transformed body: ${nDefDef.rhs.show}", 0,  ddef.srcPos)(using ctx, tctx)
    }
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
        val (tctx, monadValDef) = makeCpsTopLevelContext(ctxRef, ddef.symbol, ddef.rhs.srcPos,
                                     DebugSettings.make(ddef, settings), CpsTransformHelper.cpsMonadContextClassSymbol)
        val ddefContext = ctx.withOwner(ddef.symbol)
        val nRhsCps = RootTransform(ddef.rhs, ddef.symbol, 0)(using ddefContext, tctx)
        val nRhs = Block(monadValDef.changeOwner(monadValDef.symbol.owner,ddef.symbol)::Nil, nRhsCps.transformed(using ddefContext, tctx))
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
            throw CpsTransformException(s"unsupported type for top-level wrap, asyncKind=${cpsTree.asyncKind}", cpsTree.origin.srcPos)
  }


  /**
   * create cps top-level context for transformation.
   * @param cpsDirectOrSimpleContext - reference to cpsMonadContext.
   * @param srcPos - position whre show monadInit
   **/
  private def makeCpsTopLevelContext(cpsDirectOrSimpleContext: Tree, owner:Symbol, srcPos: SrcPos, debugSettings:DebugSettings, wrapperSym: Symbol)(using Context): (CpsTopLevelContext, ValDef) =  {
    cpsDirectOrSimpleContext match
      case vd: ValDef =>
        throw CpsTransformException("incorrect makeCpsTopLevelContext", srcPos)
      case _ =>
    //val monadInit = Select(cpsDirectOrSimpleContext, "monad".toTermName).withSpan(srcPos.span)
    val monadType = CpsTransformHelper.extractMonadType(cpsDirectOrSimpleContext.tpe.widen, wrapperSym, srcPos)
    val monadInit = genMonadInit(cpsDirectOrSimpleContext, srcPos, debugSettings, wrapperSym, monadType)

    val optRuntimeAwait = if(settings.useLoom) CpsTransformHelper.findRuntimeAwait(monadType, srcPos.span) else None
    val optRuntimeAwaitProvider = if (settings.useLoom) CpsTransformHelper.findRuntimeAwaitProvider(monadType, srcPos.span) else None
    val monadValDef = SyntheticValDef("m".toTermName, monadInit)(using summon[Context].fresh.setOwner(owner))
    val monadRef = ref(monadValDef.symbol)
    val optThrowSupport = CpsTransformHelper.findCpsThrowSupport(monadType, srcPos.span)
    val optTrySupport = CpsTransformHelper.findCpsTrySupport(monadType, srcPos.span)
    val isBeforeInliner = if (runsBefore.contains(Inlining.name)) { true }
                          else if (runsAfter.contains(Inlining.name)) { false }
                          else
                             throw CpsTransformException("plugins runsBefore/After Inlining not found", srcPos)
    val tc = CpsTopLevelContext(monadType, monadRef, cpsDirectOrSimpleContext,
                                optRuntimeAwait, optRuntimeAwaitProvider,
                                optThrowSupport, optTrySupport,
                                debugSettings, settings, isBeforeInliner)
    (tc, monadValDef)
  }

  private def findCpsDirectArg(app: Tree)(using Context): Option[Tree]  = {
    app match
      case app: Apply =>
        app.args.find(a => CpsTransformHelper.isCpsDirectType(a.tpe)).orElse {
          findCpsDirectArg(app.fun)
        }
      case TypeApply(fun, args) =>
        findCpsDirectArg(fun)
      case Inlined(call,bindings, expansion) =>
        findCpsDirectArg(expansion)
      case _ =>
        None
  }

  private def genMonadInit(cpsDirectOrSimpleContext: Tree,srcPos: SrcPos, debugSettings:DebugSettings, wrapperSym: Symbol, monadType:Type)(using Context): Tree = {
    if (wrapperSym.isOpaqueAlias) {
      // then we should apply extension
      val dc = cpsDirectOrSimpleContext
      val method = ref(Symbols.requiredMethod("cps.CpsDirect.monad")).withSpan(srcPos.span)
      Apply(TypeApply(method,List(TypeTree(monadType))),List(dc)).withSpan(srcPos.span)
    } else {
      val monadInit = Select(cpsDirectOrSimpleContext, "monad".toTermName).withSpan(srcPos.span)
      monadInit
    }
  }

}

object PhaseCps {

  def name: String = "rssh.cps"

}
