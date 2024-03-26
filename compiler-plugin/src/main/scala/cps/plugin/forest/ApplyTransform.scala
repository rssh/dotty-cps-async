package cps.plugin.forest

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import core.{Symbols, *}
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.StdNames.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span
import core.Types.*
import core.Phases.*
import cps.plugin.{AsyncKind, *}
import cps.plugin.forest.application.*
import QuoteLikeAPI.*
import cps.plugin.AsyncKind.Async
import cps.plugin.forest.application.ShiftedArgumentsPlainParamsShape.EXTRA_FIRST_PARAM
import cps.plugin.forest.application.ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS
import cps.{CpsMonadContext, CpsMonadConversion}
import inlines.Inlines
import transform.Inlining

import scala.collection.immutable.List
import scala.util.boundary
import scala.util.boundary.break


object ApplyTransform {

  sealed trait MbShiftedFun {
    def show(using Context): String
  }

  case class NonShiftedFun(tree: Tree) extends MbShiftedFun {
    def show(using Context): String = s"NonShiftedFun(${tree.show})"
  }

  case class ShiftedFun(
                         originTree: Tree,
                         obj: Tree,
                         method: TermName,
                         targs: List[Tree],
                         additionalArgs: Option[List[Tree]],
                         canBeOverloaded: Boolean,
                         callShouldBeInlined: Boolean,
                         /**
                            * Shifted arguments shape, need be applied after tree.
                            */
                         remainingShapeChange: ShiftedArgumentsShape
                         ) extends MbShiftedFun {

    def show(using Context): String = s"ShiftedFun(${obj.show}.${method.show}[${targs.map(_.tpe.show)}],$canBeOverloaded,$callShouldBeInlined, $remainingShapeChange)"


  }


  case class FunCallMode(
                          funKind: AsyncKind,
                          //preliminaryResultKind: AsyncKind,
                          argCallMode: ApplyArgCallMode,
                          asyncLambdaApplication: Option[AsyncKind],
                          addMonadToFirstArgList: Boolean,
                          fromCallChain: Boolean
                          )

  def apply(term: Apply, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"Apply: origin=${term.show}", nesting)


    val cpsTree = term match
        case Apply(Apply(TypeApply(fCpsAwaitCn,List(tf,ta,tg)),List(fa)), List(gc,gcn)) =>
             Log.trace(s"cpsAwait form at : ${term.show},  symbol=${fCpsAwaitCn.symbol}", nesting)
             if fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.cpsAwait") ||
                fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.await")        then
                //def cpsAwait[F[_], A, G[_]](fa: F[A])(using CpsMonadContext[G], CpsMonadConversion[F, G]): A =
                Log.trace(s"cpsAwait: ${term.show}", nesting,term.srcPos)
                AwaitTransform.fromApply(term, owner, nesting, tf, ta, tg, fa, gc, gcn)
             else
               Log.trace(s"cpsAwait not recognized",nesting)
               applyMArgs(term, owner, nesting, Nil)
        case Apply(TypeApply(adoptCpsedCallCn,List(tf,ta)),List(a))
                  if (adoptCpsedCallCn.symbol == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")) =>
              //  this means that we walk over nesting async.
              //  leave one unchanged
              Log.trace(s"adoptCpsedCall form at : ${term.show}", nesting)
              CpsTree.unchangedPure(term, owner)        
        case Apply(TypeApply(fun@adoptCpsedCallCn,List(tf,ta)),List(a))
              if (adoptCpsedCallCn.symbol == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCallCompileTimeOnly")) =>
              //  macro generate compile-time only variant of adoptCpsedCall to be stopped if plugin is not present.
              //  change to adoptCpsedCall
              Log.trace(s"adoptCpsedCallCompileTimeOnly form at : ${term.show}", nesting)
              val nFun = ref(Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")).withSpan(fun.span)
              val nTree = Apply(TypeApply(nFun,List(tf,ta)),List(a)).withSpan(term.span)
              CpsTree.pure(term, owner, nTree)
        case Apply(Apply(TypeApply(fAsynchronizedCm,List(tf,ta)),List(a)),List(fctx))
                         if (fAsynchronizedCm.symbol == Symbols.requiredMethod("cps.asynchronized")) =>
              Log.trace(s"asynchronized at : ${term.show}", nesting)
              AsynchronizedTransform.fromApply(term, owner, nesting, tf, ta, a, fctx)
              
        case Apply(cnThrow, List(_)) if (cnThrow.symbol == defn.throwMethod) =>
              ThrowTransform(term, owner, nesting)
        case Apply(TypeApply(nonLocalRecturnCn, List(targ)), List(arg))
                         if (nonLocalRecturnCn.symbol == Symbols.requiredMethod("scala.util.control.NonLocalReturns.returning")) =>
              NonLocalReturnsReturningTransform.apply(term, owner, nesting, targ, arg)
        case Apply(Apply(TypeApply(throwReturnCn, targs2), List(arg2) ), List(arg1))
                if (throwReturnCn.symbol == Symbols.requiredMethod("scala.util.control.NonLocalReturns.throwReturn")) =>
              NonLocalReturnsThrowReturnTransform.apply(term, owner, nesting, throwReturnCn, targs2, arg2, arg1)
        case _ =>
            if (summon[CpsTopLevelContext].isBeforeInliner && atPhase(inliningPhase)(Inlines.needsInlining(term))) {
              val inlined = atPhase(inliningPhase)(Inlines.inlineCall(term))
              RootTransform(inlined, owner, nesting)
            }else {
              applyMArgs(term, owner, nesting, Nil)
            }
    Log.trace(s" Apply result: ${cpsTree.show}", nesting)
    cpsTree
  }


  def applyMArgs(term: Apply, owner: Symbol, nesting:Int, tail:List[ApplyArgList] )(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"ApplyMArgs, term=${term.show}",nesting)
    val argList = makeArgList(term, MethodParamsDescriptor(term.fun), owner, nesting)
    val retval = term.fun match
      case tfa@Apply(fun1,args1) =>
        applyMArgs(tfa, owner, nesting,  argList::tail)
      case tpfa@TypeApply(tapp:Apply, targs1) =>
        val targs = makeTypeArgList(tpfa)
        applyMArgs(tapp, owner, nesting, targs::argList::tail)
      case _ =>
        parseApplication(term, owner, nesting, argList::tail)
    retval
  }



  def parseApplication(appTerm: Apply, owner: Symbol, nesting: Int, argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
    // remove case, when we have async lamba apply call.
    Log.trace(s"ApplyTransfopm.parseApplication  fun=: ${appTerm.fun.show}", nesting)
    Log.trace(s"ApplyTransfopm.parseApplication  fun.tree=: ${appTerm.fun}", nesting)
    Log.trace(s"ApplyTransfopm.parseApplication  argss.size=: ${argss.size}", nesting)
    Log.trace(s"ApplyTransfopm.parseApplication  appTerm=: ${appTerm.show}", nesting)

    val fullAppTerm = argss.last match
      case ApplyTermArgList(origin,args) => origin
      case _ => appTerm

    appTerm.fun match {
      case tpa@TypeApply(sel@Select(obj,method),targs) =>
        parseMethodCall(appTerm,owner, nesting, obj,sel,Some(tpa), argss)
      case sel@Select(obj,method) =>
        if (sel.symbol == defn.Boolean_&& || sel.symbol == defn.Boolean_||) then
          BooleanShortcutsTransform(appTerm, owner, nesting, obj, sel.symbol)
        else
          parseMethodCall(appTerm,owner, nesting, obj,sel,None, argss)
      case _ =>
        parseApplicationNonLambda(appTerm, owner, nesting, argss)
    }
  }



  def parseMethodCall(appTerm: Apply, owner: Symbol, nesting: Int, obj: Tree, sel: Select, optTypeApply:Option[TypeApply], argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {

    val cpsObjOrChain = RootTransform(obj,owner, nesting+1)

    Log.trace(s"parseMethodCall: cpsObjOrChain=${cpsObjOrChain.show}, sel.name=${sel.name}  argss=${argss.map(_.show)}, optTypeApply=${optTypeApply.map(_.show)}", nesting)
    val (cpsObj, fromCallChain) = cpsObjOrChain match
      case CallChainSubstCpsTree(origin, owner, call) =>
        (call, true)
      case _ =>
        (cpsObjOrChain,false)
    val retval = cpsObj.asyncKind match
      case AsyncKind.Sync =>
        val syncFun = optTypeApply match
          case Some(ta) => cpsObj.select(sel).typeApply(ta)
          case None => cpsObj.select(sel)
        val callMode = FunCallMode(AsyncKind.Sync, ApplyArgCallMode.SYNC, None, false, fromCallChain)
        parseSyncFunApplication(appTerm, owner, nesting, syncFun.unpure.get, argss, callMode)
      case AsyncKind.Async(internalKind) =>
        val valDefSym = newSymbol(owner, "xApplySelect".toTermName, Flags.EmptyFlags,
          cpsObj.originType.widen, Symbols.NoSymbol)
        val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
        val valRef = ref(valDefSym)
        val synFun0 = Select(valRef,Types.TermRef(cpsObj.originType,sel.symbol))
        val syncFun = optTypeApply match
          case Some(ta) => TypeApply(synFun0,ta.args)
          case None => synFun0
        val callMode = FunCallMode(cpsObj.asyncKind, ApplyArgCallMode.SYNC, None, false, fromCallChain)
        val appCpsTree = parseSyncFunApplication(appTerm, owner, nesting, syncFun, argss, callMode)
        val retval = appCpsTree match
          case CallChainSubstCpsTree(origin, owner, call) =>
            val withoutChain = call.asyncKind match
              case AsyncKind.Sync =>
                MapCpsTree(appTerm, owner, cpsObj, MapCpsTreeArgument(Some(valDef), call))
              case AsyncKind.Async(internalKind) =>
                FlatMapCpsTree(appTerm, owner, cpsObj, FlatMapCpsTreeArgument(Some(valDef), call))
              case AsyncKind.AsyncLambda(_) =>
                throw CpsTransformException("Call chain expression should not be lambda", appTerm.srcPos)
            CallChainSubstCpsTree(appTerm, owner, withoutChain)
          case _ =>
            appCpsTree.asyncKind match
              case AsyncKind.Sync =>
                MapCpsTree(appTerm, owner, cpsObj, MapCpsTreeArgument(Some(valDef), appCpsTree))
              case AsyncKind.Async(internalKind) =>
                FlatMapCpsTree(appTerm, owner, cpsObj, FlatMapCpsTreeArgument(Some(valDef), appCpsTree))
              case AsyncKind.AsyncLambda(bodyKind) =>
                MapCpsTree(appTerm, owner, cpsObj, MapCpsTreeArgument(Some(valDef), appCpsTree))
        retval
      case AsyncKind.AsyncLambda(bodyKind) =>
        cpsObj.unpure match
          case Some(lambda) =>
            val syncFun = if (cpsObj.isOriginEqSync) {
              optTypeApply match
                case Some(ta) => if (cpsObj.isOriginEqSync) ta else TypeApply(sel, ta.args)
                case None => sel
            } else {
              optTypeApply match
                case Some(ta) => cpsObj.select(sel).typeApply(ta).unpure.get
                case None => cpsObj.select((sel)).unpure.get
            }
            val callMode = FunCallMode(AsyncKind.Sync, ApplyArgCallMode.SYNC, None, false, fromCallChain)
            parseSyncFunApplication(appTerm, owner, nesting, syncFun, argss, callMode)
          case None =>
            if (sel.name == nme.apply) {
              if (optTypeApply.isDefined) then
                throw CpsTransformException("TypeApply is not supported for apply on async lambda", appTerm.srcPos)
              bodyKind match {
                case AsyncKind.Sync =>
                  // this means, that it is reality not async-lambda. (never happens)
                  throw CpsTransformException("Impossible: async-lambda kind with sync body", appTerm.srcPos)
                case AsyncKind.Async(internalKind) =>
                  if (internalKind != AsyncKind.Sync) {
                    throw new CpsTransformException("Shape is not supported yet", appTerm.srcPos)
                  }
                  val nLambda = cpsObj.transformed
                  Log.trace(s"ApplyTransform.parseApplication, nLambda=${nLambda.show}", nesting)
                  Log.trace(s"ApplyTransform.parseApplication, nLambda.tree=${nLambda}", nesting)
                  val nFun = Select(nLambda, nme.apply).withSpan(appTerm.span)
                  val callMode = FunCallMode(AsyncKind.Sync, ApplyArgCallMode.SYNC, Some(bodyKind), false, fromCallChain)
                  parseSyncFunApplication(appTerm, owner, nesting, nFun, argss, callMode)
                case AsyncKind.AsyncLambda(bodyKind2) =>
                  throw CpsTransformException("Shape (async labda which returns async lambda) is notsupported yet", appTerm.srcPos)
              }
            } else {
              //  TODO:  implement andThen .. etc
              throw CpsTransformException("Only apply is supported for async lambda now", appTerm.srcPos)
            }
    Log.trace(s"ApplyTransform.parseMethodCall result: ${retval.show}", nesting)
    retval
  }

  def parseApplicationNonLambda(appTerm: Apply, owner: Symbol, nesting:Int, argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
    val cpsApplicantNoChain = RootTransform(appTerm.fun, owner, nesting+1)
    val (cpsApplicant, fromCallChain) = cpsApplicantNoChain match
      case CallChainSubstCpsTree(origin, owner, call) =>
        (call , true)
      case _ =>
        (cpsApplicantNoChain,false)
    val callMode = FunCallMode(cpsApplicant.asyncKind, ApplyArgCallMode.SYNC, None, false, fromCallChain)
    parseApplicationCpsFun(appTerm, owner, nesting, cpsApplicant, argss, callMode)
  }


  def parseApplicationCpsFun(appTerm: Apply,
                             owner: Symbol,
                             nesting: Int,
                             cpsFun: CpsTree,
                             argss: List[ApplyArgList],
                             callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
    cpsFun.asyncKind match
      case AsyncKind.Sync =>
        val syncFun = cpsFun.unpure.get
        parseSyncFunApplication(appTerm, owner, nesting, syncFun, argss, callMode)
      case AsyncKind.Async(internalKind) =>
        val valDefSym = newSymbol(owner, "xApplyFun".toTermName, Flags.EmptyFlags,
                                  cpsFun.originType.widen, Symbols.NoSymbol)
        val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
        val valRef = ref(valDefSym)
        val appCpsTree = parseSyncFunApplication(appTerm, owner, nesting, valRef, argss, callMode)
        //  TODO:  check case when we returns async lambda.
        //   Then result will be F[AsyncLambda], which looks like our map case.
        //    The question,  is it safe to keep the function i F and call it in each application?
        //    If monad is referential transparent, then it is safe. If not - question.
        appCpsTree.unpure match
          case Some(syncAppCps)=>
              MapCpsTree(appTerm, owner, cpsFun, MapCpsTreeArgument(Some(valDef), appCpsTree))
          case None =>
              FlatMapCpsTree(appTerm, owner, cpsFun, FlatMapCpsTreeArgument(Some(valDef), appCpsTree))
      case AsyncKind.AsyncLambda(body) =>
        Log.info(s"ApplyTransform.parseApplicantCpsFun  appTerm=${appTerm.show}", nesting)
        Log.info(s"ApplyTransform.parseApplicantCpsFun  cpsFun=${cpsFun.show}", nesting)
        throw CpsTransformException("Impossible situatuon: call of AsyncLambda without apply method", appTerm.srcPos)
  }

  /**
   *
   *
   * @param origin
   * @param owner
   * @param nesting
   * @param fun
   * @param argss list of arguments, starting from the first non-type argument lists.
   * @param callMode
   * @return
   */
  def parseSyncFunApplication(origin: Apply, owner:Symbol, nesting: Int, fun: Tree, argss:List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
      val tctx = summon[CpsTopLevelContext]
      val runShiftAsyncLambda = argss.exists(_.containsNotUnshiftableAsyncLambda)
      val containsAsync = argss.exists(_.isAsync)
      val retval = if (runShiftAsyncLambda) {
        if (tctx.pluginSettings.runtimeAwaitBeforeCps  && tctx.supportsRuntimeAwait) then
            genApplicationWithRuntimeAwait(origin, owner, nesting, fun, argss, callMode)
        else
            if (fun.denot != NoDenotation) {
                  // check -- can we add shifted version of fun
                  retrieveShiftedFun(origin,fun,owner, argss) match
                    case Right(newFun) =>
                      val newCallMode = FunCallMode(AsyncKind.Sync, ApplyArgCallMode.ASYNC_SHIFT, None,
                                          newFun.remainingShapeChange.p == ShiftedArgumentsPlainParamsShape.EXTRA_FIRST_PARAM,
                                          callMode.fromCallChain)
                      genApplication(origin, owner, nesting, newFun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC_SHIFT, None), newCallMode)
                    case Left(error) =>
                      if (!tctx.pluginSettings.runtimeAwaitBeforeCps || !tctx.supportsRuntimeAwait) then
                        throw CpsTransformException(error, origin.srcPos)
                      else
                        genApplicationWithRuntimeAwait(origin, owner, nesting, fun, argss, callMode)
            } else {
                throw CpsTransformException(s"Can't transform function ${fun}",fun.srcPos)
            }
      } else if (containsAsync) {
        genApplication(origin, owner, nesting, NonShiftedFun(fun), argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, None), callMode)
      } else {
        parseSyncFunPureApplication(origin, owner, nesting, fun, argss, callMode)
      }
      retval
  }



  //  just unchanged
  // TODO: pass internal async-kine in impure parameter
  def parseSyncFunPureApplication(origin: Apply, owner: Symbol, nesting: Int, fun: Tree, argss:List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
     Log.trace(s"parseSyncFunPureApplication: fun=${fun.show}", nesting)
     val plainTree = argss.foldLeft(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Log.trace(s"parseSyncFunPureApplication.fold,  s=${s.show}, args=${args.map(_.show)}", nesting)
            val exprsInCalls = args.map(_.exprInCall(ApplyArgCallMode.SYNC,None))
            Log.trace(s"parseSyncFunPureApplication.fold,  exprsInCalls=${exprsInCalls.map(_.show)}", nesting)
            Log.trace(s"parseSyncFunPureApplication.fold,  exprsInCalls==args: ${exprsInCalls == args.map(_.origin)}",nesting)
            // workarrond for https://github.com/lampepfl/dotty/issues/18113
            //  TODO: eliminate after implementing eta-expansion
            val s1 = s match
              case Block(head::tail,fun) =>
                Apply(Inlined(s,List.empty,s),exprsInCalls).withSpan(orig.span)
              case _ =>
                Apply(s,exprsInCalls).withSpan(orig.span)
            Log.trace(s"parseSyncFunPureApplication.fold,  s1=${s1.show}", nesting)
            s1
     }
     val fullOrigin = if (argss.isEmpty) origin else argss.last.origin
     Log.trace(s"parseSyncFunPureApplication: plainTree=${plainTree.show}", nesting)
     val retval = adoptCallMode(fullOrigin, plainTree, owner, argss, callMode)
     Log.trace(s"parseSyncFunPureApplication: retval=${retval.show}", nesting)
     retval
  }

  def adoptCallMode(origin: Tree, plainTree: Tree, owner: Symbol, argss: List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
    if (argss.exists(_.containsDirectContext) ) {
      val directContextArg = argss.find(_.containsDirectContext).flatMap(_.findDirectContext).get
      val adoptedTree = directContextArg match
            case dc@CpsDirectHelper.ByInclusionCall(tf,tg,fctx,fgincl) =>
                 val callArgs = CpsDirectHelper.ByInclusionCallArgs(tf,tg,fctx,fgincl)
                 if (tf.tpe =:= tg.tpe) then
                   val nCpsDirectArg = CpsDirectHelper.genCpsDirectDefaultConstructor(TypeTree(tf.tpe),fctx,dc.span)
                   val tree = CpsDirectHelper.substituteCpsDirectArgInCall(plainTree, callArgs, nCpsDirectArg).getOrElse(
                     throw CpsTransformException("Internal error: can't find direct context argument in call", origin.srcPos)
                   )
                   if (!summon[CpsTopLevelContext].pluginSettings.transformDirectContextLambda && callMode.asyncLambdaApplication.isDefined) then
                    // lambda used only here, so don't need to preserve call.
                    // also all internal labdas are rewritten, so symbol of call will be other
                    // So, if compiling of async-lambda call not cause loading of symbol, then
                    //  we can don't transfrom internal direct context lambda at all.
                    tree
                   else
                    Scaffolding.adoptCpsedCall(tree, plainTree.tpe.widen, summon[CpsTopLevelContext].monadType)
                 else
                   //
                   if (callMode.asyncLambdaApplication.isDefined) then
                     throw CpsTransformException("Lambda application to direct call of other monad is not supported yet",origin.srcPos)
                   val mt = MethodType(List("ctx".toTermName))(
                     _ => List(Symbols.requiredClassRef("cps.CpsTryMonadContext").appliedTo(List(tg.tpe.widen))),
                     _ => tg.tpe.widen.appliedTo(List(origin.tpe.widen))
                   )
                   val sym = newAnonFun(owner, mt)
                   val lambda = Closure(sym, tss => {
                      val List(ctx) = tss.head
                      val nCpsDirectArg = CpsDirectHelper.genCpsDirectDefaultConstructor(TypeTree(tg.tpe),ctx,dc.span)
                      val tree = CpsDirectHelper.substituteCpsDirectArgInCall(plainTree, callArgs, nCpsDirectArg).getOrElse {
                        throw CpsTransformException("Internal error: can't find direct context argument in call when building inclusion", origin.srcPos)
                      }
                      Scaffolding.adoptCpsedCall(tree, tree.tpe.widen, tg.tpe.widen)
                   })
                   CpsDirectHelper.genConventionCall(fctx,fgincl,origin.tpe.widen,lambda,origin.span)
            case other =>
              Scaffolding.adoptCpsedCall(plainTree, plainTree.tpe.widen, summon[CpsTopLevelContext].monadType)
      //if (isImpure) {
      //  TODO: such situationis possible when we pass lamba with context parameters (can be inline)
      //  TODO:  separate this case.
      //  throw CpsTransformException(s"Impure call in combination of callign context functon is not supported yet", origin.srcPos)
      //}
      /**
      callMode.preliminaryResultKind match
        case AsyncKind.Sync => AsyncKind.Sync
        case AsyncKind.Async(internalKind) => internalKind
        case AsyncKind.AsyncLambda(bodyKind) => AsyncKind.Sync
      CpsTree.impure(origin, owner, adoptedTree, internalKind)
      */
      //CpsTree.impure(origin, owner, adoptedTree, AsyncKind.Sync)
      adoptResultKind(origin, adoptedTree, owner, callMode, true)
    } else {
      adoptResultKind(origin, plainTree, owner, callMode, false)
    }
  }



  def adoptResultKind(origin:Tree, newApply: Tree, owner: Symbol, callMode: FunCallMode, usingDirectContext: Boolean)(using Context, CpsTopLevelContext): CpsTree = {

    if (callMode.argCallMode == ApplyArgCallMode.ASYNC_SHIFT || callMode.fromCallChain) {
        if (newApply.tpe.baseType(Symbols.requiredClass("cps.runtime.CallChainAsyncShiftSubst"))!=NoType) {
          if (usingDirectContext) {
            throw CpsTransformException("DirectContext function can not return CallChaninAsyncShiftSubst", origin.srcPos)
          } else {
            CallChainSubstCpsTree(origin, owner, CpsTree.pure(origin, owner, newApply))
          }
        } else {
          val originType = origin.tpe.widen
          val newType = newApply.tpe.widen
          if (originType =:= newType) {
            if (usingDirectContext) then
              CpsTree.impure(origin,owner,newApply,AsyncKind.Sync)
            else
              CpsTree.pure(origin,owner,newApply)
          } else if (newType <:< summon[CpsTopLevelContext].monadType.appliedTo(WildcardType)) {
            val adoptedApply =  
              if (usingDirectContext) {
                report.warning("async-shifted function with direct context return wrapped type", origin.srcPos)
                Apply(
                  TypeApply(
                    Select(summon[CpsTopLevelContext].cpsMonadRef, "flatten".toTermName),
                    List(TypeTree(originType.widen))
                  ),
                  List(newApply)
                )
              } else {
                newApply
              }
            CpsTree.impure(origin, owner, adoptedApply, AsyncKind.Sync)
          } else if (callMode.asyncLambdaApplication.isDefined) {
            if (usingDirectContext) {
              throw CpsTransformException("Lambda applications can't be used with direct context", origin.srcPos)
            }
            CpsTree.impure(origin, owner, newApply, callMode.asyncLambdaApplication.get)
          } else {
            // TODO: warn about possible unsafe result type
            if (usingDirectContext) {
              CpsTree.impure(origin,owner,newApply,AsyncKind.Sync)
            } else {
              CpsTree.pure(origin, owner, newApply)
            }
          }
        }
    } else if (callMode.asyncLambdaApplication.isDefined) {
       //  asynk lambda can be applied to direct context (as the result of the inline function)
       //  question - are we transform lambda-applications with direct context or not.
       //  current solution: use flag in plugin settings.
       val transformDirectContextLambdaCall = summon[CpsTopLevelContext].pluginSettings.transformDirectContextLambda
       callMode.asyncLambdaApplication.get match
         case AsyncKind.Sync =>
           if (usingDirectContext && transformDirectContextLambdaCall) {
             CpsTree.impure(origin,owner,newApply,AsyncKind.Sync)
           } else {
             CpsTree.pure(origin,owner,newApply)
           }
         case  AsyncKind.Async(internalKind) =>
           if (usingDirectContext && transformDirectContextLambdaCall) {
             val flattenedNewApply = Apply(
               TypeApply(
                 Select(summon[CpsTopLevelContext].cpsMonadRef, "flatten".toTermName),
                 List(TypeTree(newApply.tpe.widen))
               ),
               List(newApply)
             )
             CpsTree.impure(origin,owner,flattenedNewApply,internalKind)
           } else {
             CpsTree.impure(origin, owner, newApply, internalKind)
           }
         case AsyncKind.AsyncLambda(bodyKind) =>
           if (usingDirectContext && transformDirectContextLambdaCall) {
             // it's why better to keep transformDirectContextLambdaCall = false
             throw CpsTransformException("Unsuppored use of lamba application as output of direct context lambda", origin.srcPos)
           }
           CpsTree.opaqueAsyncLambda(origin,owner,newApply,bodyKind)
    } else if (usingDirectContext) {
       CpsTree.impure(origin,owner,newApply, AsyncKind.Sync)
    } else {
       CpsTree.pure(origin,owner,newApply)
    }


    /*
    preliminaryResultKind match
      case AsyncKind.Sync =>
        if ((callMode.argCallMode == ApplyArgCallMode.ASYNC_SHIFT || callMode.fromCallChain)
          && newApply.tpe.baseType(Symbols.requiredClass("cps.runtime.CallChainAsyncShiftSubst"))!=NoType) {
          CallChainSubstCpsTree(origin, owner, CpsTree.pure(origin, owner, newApply))
        } else if (callMode.fromCallChain) {
          //TODO: determiante kinf with lambda-s from result type
          //val asyncKind = CpsTransformHelper.kindFromType(newApply.tpe.widen)
          if (newApply.tpe.widen =:= origin.tpe.widen) {
            CpsTree.pure(origin,owner,newApply)
          } else if ( newApply.tpe.baseType(summon[CpsTopLevelContext].monadType.typeSymbol) != NoType ) {
            CpsTree.impure(origin,owner,newApply,AsyncKind.Sync)
          } else {
            // TODO: check method type
            CpsTree.pure(origin,owner,newApply)
          }
        } else {
          PureCpsTree(origin, owner, newApply)
        }
      case AsyncKind.Async(internalKind) =>
        CpsTree.impure(origin, owner, newApply, internalKind)
      case AsyncKind.AsyncLambda(bodyKind) =>
        CpsTree.opaqueAsyncLambda(origin, owner, newApply, bodyKind)

     */
  }

  def genApplication(origin:Apply, owner: Symbol, nesting: Int, fun: MbShiftedFun, argss: List[ApplyArgList], f: ApplyArg => Tree, callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"genApplication origin: ${origin.show}", nesting)
    Log.trace(s"genApplication fun=${fun}", nesting)
    Log.trace(s"genApplication argss=${argss.map(_.show)}", nesting)


    def genOneLastPureApply(fun: Tree, argList: ApplyArgList, argListIndex:Int): Tree = {
      val tree = argList match
        case ApplyTypeArgList(origin, targs) =>
          TypeApply(fun, targs).withSpan(origin.span)
        case ApplyTermArgList(origin, args) =>
          // workarrond for https://github.com/lampepfl/dotty/issues/18113
          //  (obsotete,  now we do eta-expansion, but need retest before removing)
          val mappedArgs = args.map(f)
          val nArgs = if (argListIndex == 0 && callMode.addMonadToFirstArgList) {
                          summon[CpsTopLevelContext].cpsMonadRef :: mappedArgs
                      } else {
                          mappedArgs
                      }
          val fun1 = fun match
            case Block(head::tail,fun) =>
               // TODO: Select(fun,"apply") instead of Inlined (see discussion in ticket)
               Inlined(fun,List.empty,fun)
            case _ =>
                fun
          Apply(fun1, nArgs).withSpan(origin.span)
      tree
    }

    @tailrec
    def genPureReply(fun:Tree, argss: List[ApplyArgList], index:Int): Tree =
      argss match
        case Nil => fun
        case head::tail => genPureReply(genOneLastPureApply(fun, head, index),tail, index+1)

    def genOneLastMbShiftedApply(fun: MbShiftedFun, argList: ApplyArgList, argListIndex:Int): Tree = {

      def assembleNonOverloadedShifted(fun: ShiftedFun): Tree = {
        val pre0 = Select(fun.obj,fun.method.toTermName)
        val pre1 = if (fun.targs.isEmpty) pre0 else TypeApply(pre0, fun.targs)
        if (pre1.tpe.isError) {
          throw CpsTransformException(s"assembleNonOverloadedShifted is error, fun=${fun.show}, generated fun = ${pre1.show}", fun.originTree.srcPos)
        }
        val pre2 = fun.additionalArgs match
          case None => pre1
          case Some(args) => Apply(pre1, args)
        val retval = pre2.withSpan(fun.originTree.span)
        if (retval.tpe.isError) {
           throw CpsTransformException(s"assembleNonOverloaded is error, fun=${fun.show}, generated fun = ${retval.show}", fun.originTree.srcPos)
        }
        retval
      }

      fun match
        case NonShiftedFun(tree) =>
          genOneLastPureApply(tree, argList, argListIndex)
        case sf@ShiftedFun(origin, obj, method, targs, additionalArgs, canBeOverloaded, callShouldBeInlined, shape) =>
          val tree = argList match
            case ApplyTypeArgList(origin, targs1) =>
              TypeApply(assembleNonOverloadedShifted(sf), targs1).withSpan(origin.span)
            case ApplyTermArgList(origin, args0) =>
              val args =
                if (argListIndex == 0 && callMode.addMonadToFirstArgList) then
                  summon[CpsTopLevelContext].cpsMonadRef :: args0.map(f)
                else
                  args0.map(f)
              if (canBeOverloaded) then
                if (additionalArgs.isEmpty) then
                  applyOverloaded(obj, method, args, targs.map(_.tpe), Types.WildcardType).withSpan(origin.span)
                else
                  val alternatives = obj.tpe.member(method).alternatives
                  val selected = alternatives.filter { a =>
                    a.info match
                      case pt: PolyType =>
                        // TODO: check types?
                        val step1 = (pt.typeParams.length == targs.length)
                        val step2 = pt.resType match
                          case rmt: MethodType =>
                            rmt.paramInfos.length == 2 && {
                              rmt.resType match
                                case rmt2: MethodType =>
                                  rmt2.paramInfos.length == args.length
                                case _ =>
                                  false
                            }
                          case _ =>
                            false
                        step1 && step2
                      case mt: MethodType =>
                        mt.paramInfos.length == args.length
                      case _ =>
                        throw CpsTransformException(s"unexpected type of method ${a.show}: ${a.info.show}, expected MethodType or PolyType", origin.srcPos)
                  }
                  if (selected.isEmpty) {
                    // TODO: log failed
                    throw CpsTransformException(s"no suitable alternative for ${obj.show}.${method.show} with ${args.length} arguments", origin.srcPos)
                  } else if (selected.tail.nonEmpty) {
                    // this will wrote an error.
                    val f1 = applyOverloaded(obj, method, additionalArgs.get, targs.map(_.tpe), Types.WildcardType)
                    Apply(f1, args).withSpan(origin.span)
                    throw CpsTransformException(s"more than one alternative for ${obj.show}.${method.show} with ${args.length} arguments", origin.srcPos)
                  } else {
                    val denotation = selected.head
                    val sel = Select(obj, denotation.symbol.namedType)
                    val pre0 = if (targs.isEmpty) sel else TypeApply(sel, targs)
                    val pre1 = Apply(pre0, additionalArgs.get)
                    Apply(pre1, args).withSpan(origin.span)
                  }
              else
                Apply(assembleNonOverloadedShifted(sf), args).withSpan(origin.span)
          tree
    }

    def genMbShiftedReply(fun: MbShiftedFun, argss: List[ApplyArgList], index: Int): Tree =
      argss match
        case Nil => throw CpsTransformException("argument list should not be empty", origin.srcPos)
        case head :: tail => genPureReply(genOneLastMbShiftedApply(fun, head, index), tail, index + 1)


    def genOneApplyPrefix(origin: Tree, args:List[ApplyArg], tailCpsTree:CpsTree): CpsTree =
        args.foldRight(tailCpsTree) { (e,s) =>
          if (e.flatMapsBeforeCall.isEmpty) then
            s
          else
            val withPrevArgs = e.flatMapsBeforeCall.foldRight(s) { (pre, tail) =>
              val (prefixCpsTree, prefixVal) = pre
              Log.trace(s"genApplication: oneApplyPrefix for ${e.show}", nesting)

              Log.trace(s"genApplication: oneApplyPrefix prefixCpsTree=${prefixCpsTree.show}", nesting)
              Log.trace(s"genApplication: oneApplyPrefix prefixCpsTree.transformed=${prefixCpsTree.transformed.show}", nesting)
              Log.trace(s"genApplication: oneApplyPrefix prefixVal=${prefixVal.show}", nesting)
              e match
                case plain: PlainApplyArg =>
                  Log.trace(s"genApplication: oneApplyPrefix e.expr=${plain.expr.show}", nesting)
                  Log.trace(s"genApplication: oneApplyPrefix e.expr.transformed=${plain.expr.transformed.show}", nesting)
                  Log.trace(s"genApplication: oneApplyPrefix e.expr.kind=${plain.expr.asyncKind}", nesting)
                  Log.trace(s"e.expr.origin=${plain.expr.origin.show}", nesting)
                  Log.trace(s"e.expr.originType=${plain.expr.originType.show}", nesting)
                case _ =>

              // TODO: optimise.
              //  (mb - introduce flaMap as operations, which automatically do optimizations)
              FlatMapCpsTree(
                origin,
                owner,
                prefixCpsTree,
                FlatMapCpsTreeArgument(
                  Some(prefixVal),
                  tail
                )
              )
            }
            if (e.enclosingInlined.isEmpty) then
                withPrevArgs
            else
                wrapInInlined(e.enclosingInlined, withPrevArgs)

        }

    def genPrefixes(argss:List[ApplyArgList], tailCpsTree:CpsTree): CpsTree =
      argss.foldRight(tailCpsTree) { (e,s) =>
         e match
          case ApplyTermArgList(origin,args) =>
            genOneApplyPrefix(origin,args,s)
          case _ => s
      }

    val pureReply = genMbShiftedReply(fun, argss, 0)
    Log.trace(s"pureReply= ${pureReply.show}", nesting)
    val pureReplyMbInlined = fun match
      case shiftedFun: ShiftedFun =>
        if (shiftedFun.callShouldBeInlined && summon[CpsTopLevelContext].isAfterInliner) {
          val inlined = atPhase(inliningPhase){
             Inlines.inlineCall(pureReply)
          }
          inlined
        } else {
          pureReply
        }
      case _ => pureReply
    val fullOrigin = if (argss.isEmpty) origin else argss.last.origin
    val lastCpsTree = adoptCallMode(fullOrigin, pureReply, owner, argss, callMode)
    val nApplyCpsTree = genPrefixes(argss, lastCpsTree)
    val retval = nApplyCpsTree
    Log.trace(s"genApplication result: ${retval.show}", nesting)
    //Log.trace(s"genApplication result transformed: ${retval.transformed.show}", nesting)
    Log.trace(s"genApplication exists containsMonadContext: ${argss.exists(_.containsDirectContext)}",nesting)
    retval
  }

  private def genApplicationWithRuntimeAwait(origin: Apply, owner: Symbol, nesting: Int, fun: Tree, argss: List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
     val tctx = summon[CpsTopLevelContext]
     tctx.optRuntimeAwait match
       case Some(runtimeAwait) =>
         val retval = genApplication(origin, owner, nesting, NonShiftedFun(fun), argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, Some(runtimeAwait)), callMode)
         retval
       case None =>
         tctx.optRuntimeAwaitProvider match
           case Some(runtimeAwaitProvider) =>
             val runtimeAwaitType = Symbols.requiredClassRef("cps.CpsRuntimeAwait").appliedTo(List(tctx.monadType))
             val runtimeAwaitSym = Symbols.newSymbol(owner, "runtimeAwait".toTermName, Flags.Synthetic, runtimeAwaitType , coord = origin.span)
             val runtimeAwaitFormal = ref(runtimeAwaitSym)
             val wrappedCall = genApplication(origin, owner, nesting, NonShiftedFun(fun), argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, Some(runtimeAwaitFormal)), callMode)
             val mt = MethodType(List("runtimeAwait".toTermName))(
               _ => List(runtimeAwaitType),
               _ => wrappedCall.transformedType.widen
             )
             val meth = Symbols.newAnonFun(owner, mt)
             val (raLambda, internalKind) = wrappedCall.unpure match
               case Some(syncCall) =>
                  val lambda = Closure(meth, tss => {
                    val ctx = summon[Context].withOwner(meth)
                    // we shoudl substParamsMap before changeOwner
                    val callInLambda = TransformUtil.substParamsMap(syncCall, Map(runtimeAwaitSym -> tss.head.head)).changeOwner(owner, meth)
                    {
                        given Context = ctx
                        Apply(
                          TypeApply(
                            Select(tctx.cpsMonadRef, "pure".toTermName),
                            List(TypeTree(wrappedCall.originType.widen))
                          ),
                          List(callInLambda)
                        ).withSpan(origin.span)
                    }
                  })
                  (lambda, AsyncKind.Sync)
               case None =>
                  wrappedCall.asyncKind match
                    case AsyncKind.Sync =>
                      throw CpsTransformException(s"Impossible - asyncKind=Sync with unpure=None, cpsTree=${wrappedCall.show}", origin.srcPos)
                    case AsyncKind.Async(internalKind) =>
                      if (internalKind != AsyncKind.Sync) then
                        throw CpsTransformException("Unsupported internal kind for provided runtime await", origin.srcPos)
                      val lambda = Closure(meth, tss => {
                        // we shoudl substParamsMap before changeOwner
                        TransformUtil.substParamsMap(wrappedCall.transformed, Map(runtimeAwaitSym -> tss.head.head)).changeOwner(owner, meth)
                      })
                      (lambda, internalKind)
                    case AsyncKind.AsyncLambda(bodyKind) =>
                      throw CpsTransformException("Unsupported internal kind for provided runtime await", origin.srcPos)
             val retval = Apply(
               Apply(
                 TypeApply(
                   Select(
                     runtimeAwaitProvider,
                     "withRuntimeAwait".toTermName
                   ),
                   List(TypeTree(wrappedCall.originType.widen))
                 ),
                 List(raLambda)
               ),
               List(tctx.cpsNonDirectContext)
             ).withSpan(origin.span)
             val fullOrigin = if (argss.isEmpty) origin else argss.last.origin
             CpsTree.impure(fullOrigin, owner, retval, internalKind)
           case None =>
             throw CpsTransformException(s"Can't find runtime await support for ${tctx.monadType.show}", origin.srcPos)
  }


  def makeArgList(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): ApplyTermArgList = {
    // need to calculate dependency between arguments.
    ApplyTermArgList.make(term, mt, owner: Symbol, nesting: Int)
  }


  def makeTypeArgList(term: TypeApply)(using Context): ApplyTypeArgList = {
    ApplyTypeArgList(term,term.args.map(tt => TypeTree(tt.tpe)))
  }



  /**
   * @param origin
   * @param fun - fun,  which can be withFilter invocation.
   * @param owner
   * @param Context
   * @param CpsTopLevelContext
   * @return
   */
  def retrieveShiftedFun(origin: Tree,  fun:Tree, owner: Symbol, argLists: List[ApplyArgList])(using Context, CpsTopLevelContext): Either[String,ShiftedFun] = {

    val withFilterType = Symbols.requiredClassRef("scala.collection.WithFilter").appliedTo(List(WildcardType, WildcardType))

    object WithFilterCall {
      def unapply(tree: Tree): Option[(Tree,TermName,List[Tree])] = tree match
        case Select(obj,methodName) if obj.tpe <:< withFilterType && !(obj.tpe =:= defn.NothingType) =>
          Some((obj,methodName.toTermName,List.empty))
        case TypeApply(Select(obj,methodName),targs) if obj.tpe <:< withFilterType && !(obj.tpe =:= defn.NothingType) =>
          Some((obj,methodName.toTermName,targs))
        case _ => None
    }

    fun match
      case WithFilterCall(obj,methodName,methodTypeParams) =>
        // With filter is a special case , because it is impossible to rertieve underlaying collection from WithFilter instance.
        // So, we trying to find it.withFilter invocation and substitute it with own shifted implementation
        obj match
          case Apply(Select(itObj,withFilterCn),List(predicate)) if withFilterCn == "withFilter".toTermName =>
            resolveAsyncShiftedObject(itObj) match
              case Right(itShiftedObj) =>
                //val withFilterSubstDenot = itShiftedObj.tpe.member("_cpsWithFilterSubst".toTermName)
                val withFilterSubstSelect = Select(maybeInlineObject(itShiftedObj), "_cpsWithFilterSubst".toTermName)
                val newQual = Apply(withFilterSubstSelect, List(itObj, predicate)).withSpan(itObj.span)
                val newSelect = Select(newQual, methodName)
                val nTypeParams = if (methodTypeParams.isEmpty) List.empty else TypeTree(summon[CpsTopLevelContext].monadType) :: methodTypeParams
                //val newFun0 = if (methodTypeParams.isEmpty) {
                //  newSelect
                //} else {
                //  TypeApply(newSelect, nTypeParams)
                //}
                //val newFun = Apply(newFun0,List(summon[CpsTopLevelContext].cpsMonadRef)).withSpan(fun.span)
                val retval = ShiftedFun(fun,
                  newQual,
                  methodName,
                  nTypeParams,
                  Some(List(summon[CpsTopLevelContext].cpsMonadRef)),
                  false,
                  false,
                  ShiftedArgumentsShape.same
                )
                Right(retval)
              case Left(error) =>
                Left(error)
                //throw CpsTransformException(s"Can't resolve shifted object for withFilter: ${error}", fun.srcPos)
          case _ =>
            //TODO: expand set of possible withFilter consturctors
            Left("Can't retrieve underlaying collection from WithFilter instance")
            //throw CpsTransformException("Can't retrieve underlaying collection from WithFilter instance", fun.srcPos)
      case _ => retrieveShiftedFunNoSpecial(origin, fun, owner, argLists)
  }


    /**
   * retrieve shifted function or throw exception.
   * @param fun
   * @param owner
   * @param Context
   * @param CpsTopLevelContext
   * @return  new function (with type arguments and additional parameter list if needed)
   */
  def retrieveShiftedFunNoSpecial(origin: Tree,  fun:Tree, owner: Symbol, argLists: List[ApplyArgList])(using Context, CpsTopLevelContext): Either[String,ShiftedFun] = {

    val tctx = summon[CpsTopLevelContext]

    def approxCompatibleTypes(inOrigin: Type, inCandidate: Type): Boolean = {
      val origin = inOrigin.dealias
      val candidate = inCandidate.dealias
      val retval = if (defn.isFunctionType(origin) || defn.isContextFunctionType(origin)) {
        defn.isFunctionType(candidate) //  mb in futuer check arguments for real approximation
        } else if (defn.isFunctionType(candidate)) {
          true
        } else if ( origin <:< candidate ) {
          true
        } else if (origin.baseType(candidate.typeSymbol) != NoType) {
          // bug in scala-3.3.0  ! (Seq[B] <:< IterableOnce[B]) == true
          true
        } else if (candidate.typeSymbol.isTypeParam) {
          true
        } else {
          origin match
            case AppliedType(orTycon, orTargs) =>
              candidate match
                case AppliedType(cnTycon, cnTargs) =>
                  approxCompatibleTypes(orTycon, cnTycon) &&
                    (orTargs zip cnTargs).forall { (pair) =>
                      approxCompatibleTypes(pair._1, pair._2)
                    }
                case cTypeRef:TypeRef =>
                   cTypeRef.typeSymbol.isTypeParam
                case _ =>
                  false
            case _: TermRef =>
              origin =:= candidate
            case typeRef: TypeRef =>
              if (typeRef.symbol.isTypeParam) then
                candidate.typeSymbol.isTypeParam
              else
                false
            case _ =>
              true
      }
      retval
    }

    def approxCompatibleParamList(origin: List[Symbol], candidate: List[Symbol]): Either[String,Unit] =
      if (origin.length == candidate.length) {
        var invalidParamMessage: Option[String] = None
        val retval = (origin zip candidate).exists{ (pair) =>
          if (!approxCompatibleTypes(pair._1.info, pair._2.info)) {
            invalidParamMessage = Some(s"parameter ${pair._1.name} have incompatible types ${pair._1.info.show} and ${pair._2.info.show}")
            true
          } else {
            false
          }
        }
        invalidParamMessage match
          case Some(msg) =>
            Left(msg)
          case None =>
            Right(())
      } else {
        Left("parameters count mismatch")
      }


    def matchInplaceArgTypes(originSym:Symbol, candidateSym: Symbol): Either[String,ShiftedArgumentsShape] = {

      def checkTypeArgs(originTypeParamss: List[List[Symbol]], candidateTypeParamSymms: List[List[Symbol]]): Either[String,ShiftedArgumentsTypeParamsShape] =
        println(s"CheckTypeArgs: originTypeParamss = ${originTypeParamss}, candidateTypeParamSymms = ${candidateTypeParamSymms}")
        if (candidateTypeParamSymms.isEmpty) then
          if (originTypeParamss.isEmpty) then
            Right(ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS)
          else
            Left(s"${candidateSym.name} have no type arguments")
        else if (originTypeParamss.length == candidateTypeParamSymms.length) then
          println(s"CheckTypeArgs: originTypeParamss.length==candidateTypeParamSymms.length=${originTypeParamss.length}")
          val originTpArgs = originTypeParamss.head
          val candidateTpArgs = candidateTypeParamSymms.head
          if (candidateTpArgs.length == originTpArgs.length+1) then
            //  with F[_]  in type-args
            val tp = ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM
            Right(tp)
          else if (candidateTpArgs.length == originTpArgs.length) then
            //  without F[_]  in type-args
            val tp = ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS
            Right(tp)
          else
            Left(s"${candidateSym.name} have wrong number of type arguments, origin: ${originTpArgs.length}, candidate:  ${candidateTpArgs.length}")
        else if (originTypeParamss.length+1 == candidateTypeParamSymms.length) then
          Right(ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST)
        else
          Left(s"${candidateSym.name} have wrong number of type arguments (shoule be ${originTypeParamss.length} or ${originTypeParamss.length+1})")

      def checkPlainArgss(originPlainParamss: List[List[Symbol]], candidatePlainParamss: List[List[Symbol]]): Either[String, ShiftedArgumentsPlainParamsShape] =
        if (originPlainParamss.length == candidatePlainParamss.length) then
          val originPlainArgs = originPlainParamss.head
          val candidatePlainArgs = candidatePlainParamss.head
          if (candidatePlainArgs.length == originPlainArgs.length+1) then
            if (candidatePlainArgs.head.info.baseType(Symbols.requiredClass("cps.CpsMonad") ) != NoType) then
              approxCompatibleParamList(originPlainArgs, candidatePlainArgs.tail).map(_ => ShiftedArgumentsPlainParamsShape.EXTRA_FIRST_PARAM)
            else
              Left("Type of first argument of candidate is not CpsMonad and lenght of params is different")
          else if (candidatePlainArgs.length == originPlainArgs.length) then
            approxCompatibleParamList(originPlainArgs, candidatePlainArgs).map(_ => ShiftedArgumentsPlainParamsShape.SAME_PARAMS)
          else
            Left(s"${candidateSym.name} have wrong number of arguments")
        else if (originPlainParamss.length+1 == candidatePlainParamss.length) then
          val nextCandidate = candidatePlainParamss.tail.head
          approxCompatibleParamList(originPlainParamss.head, nextCandidate).map(_ => ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST)
        else
          Left(s"${candidateSym.name} have wrong number of arguments (shoule be ${originPlainParamss.length} or ${originPlainParamss.length+1})")

      val (originTp, originPlain) = originSym.paramSymss.partition(_.exists(_.isType))
      val (candidateTp, candidatePlain) = candidateSym.paramSymss.partition(_.exists(_.isType))

      val retval = for {
        tpShape <- checkTypeArgs(originTp, candidateTp)
        plainShape <- checkPlainArgss(originPlain, candidatePlain)
      } yield ShiftedArgumentsShape(tpShape, plainShape)

      retval
    }



    def tryFindInplaceAsyncShiftedMethods(funSym: Symbol, objSym: Symbol, name: Name, suffixes: Set[String]): Either[String,Map[Symbol,ShiftedArgumentsShape]]  = {
      val shapes = (for{ m <- objSym.lookupPrefix.allMembers
                suffix <- suffixes if m.symbol.isMethod && m.symbol.name.toString == name.toString + suffix
                matchShape = matchInplaceArgTypes(fun.symbol, m.symbol)
      } yield (m.symbol, matchShape)).toMap
      val (pos, neg) = shapes.partition(_._2.isRight)
      if (pos.isEmpty) then
        Left( neg.values.map(_.left).mkString("\n") )
      else
        Right( pos.mapValues(_.right.get).toMap )
    }

    def showParamss(paramss:List[List[Symbol]])(using Context): String = {
      paramss.map{ params =>
         val isType = params.exists(_.isType)
         if (isType) {
            params.map(_.name).mkString("[",",","]")
          } else {
            params.map(x => s"${x.name}:${x.info.show}").mkString("(",",",")")
         }
      }.mkString("")
    }

    def showMethod(denotation:SymDenotation)(using Context): String = {
      s"${denotation.name}: ${denotation.info.widen.show}"
    }


    def checkAsyncShiftedMethod(originMethod: Symbol, candidateMethod: SymDenotation): Either[String,ShiftedArgumentsShape] = {


      def checkSameParameters(originNonTypeParams: List[Symbol], candidateNonTypeParams: List[Symbol]): Either[String, Boolean] = {
        approxCompatibleParamList(originNonTypeParams, candidateNonTypeParams).map(_ => true)
      }

      def checkSameParameterss(originNonTypeParamss: List[List[Symbol]], originIndex:Int,  candidateNonTypeParamss: List[List[Symbol]], candidateIndex:Int): Either[String,Boolean] =  {
         originNonTypeParamss match
           case Nil =>
             if candidateNonTypeParamss.isEmpty then Right(true) else Left("different number of parameter lists")
           case head::tail =>
             checkSameParameters(head,candidateNonTypeParamss.head) match
               case Left(err) => Left(err)
               case Right(_) => checkSameParameterss(tail, originIndex+1, candidateNonTypeParamss.tail, candidateIndex+1)
      }


      val originTpArgs = originMethod.paramSymss.head.filter(_.isType)
      if (originTpArgs.isEmpty) then
        if (candidateMethod.paramSymss.length == originMethod.paramSymss.length + 2)
          //  with extra type-arg and arglist wich pass monad
          checkSameParameterss(originMethod.paramSymss, 0, candidateMethod.paramSymss.tail.tail,2) match
            case Left(err) =>
              Left(err)
            case Right(_) => Right(ShiftedArgumentsShape(ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST, ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST))
        else
          Left(s"Can't match parameters in ${originMethod} and ${candidateMethod}")
      else if (candidateMethod.paramSymss.length == originMethod.paramSymss.length + 1)
        val candidateTpArgs = candidateMethod.paramSymss.head.filter(_.isType)
        if (candidateTpArgs.length == originTpArgs.length + 1) then
            //  with extra type-arg and arglist wich pass monad
            checkSameParameterss(originMethod.paramSymss.tail, 1, candidateMethod.paramSymss.tail.tail, 2) match
              case Left(err) => Left(err)
              case Right(_) => Right(ShiftedArgumentsShape(ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM, ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST))
        else
           Left(s"Can't match parameters in ${showMethod(originMethod)} and ${showMethod(candidateMethod)}, tpParams lenght mismatch")
      else
        Left(s"Can't match parameters in ${showMethod(originMethod)} and ${showMethod(candidateMethod)}")
    }

    def aggregate(shiftedFuns: List[ShiftedFun]): Either[String,ShiftedFun] = {
      shiftedFuns match
        case Nil => throw CpsTransformException(s"Can't find shifted function for ${fun.show}", fun.srcPos)
        case head::Nil => Right(head)
        case head::tail =>
          boundary {
            val retval = tail.foldLeft(head) { (acc, el) =>
              if (acc.remainingShapeChange != el.remainingShapeChange) then
                break(Left(s"Can't aggregate shifted functions with different shapes: ${acc} and ${el}"))
              else if (el.additionalArgs.isDefined) then
                if (acc.additionalArgs.isDefined) {
                  if (acc.additionalArgs.get.length != el.additionalArgs.get.length) then
                    break(Left(s"Can't select overloaded function with different length of additional args : ${acc} and ${el}"))
                  else if (! (acc.additionalArgs.get.zip(el.additionalArgs.get).forall{ case (l,r) => l.tpe =:= r.tpe })) then
                    break(Left(s"Can't select overloaded function with different type of additional args : ${acc} and ${el}"))
                }
                acc.copy(canBeOverloaded = true)
              else if (el.targs.length != acc.targs.length) then
                break(Left(s"Can't select overloaded function with possible different length of type-parameter lists : ${acc} and ${el}"))
              else if (! (el.targs.zip(acc.targs).forall{ case (l,r) => l.tpe =:= r.tpe })) then
                break(Left(s"Can't select overloaded function with possible different type parameters : ${acc} and ${el}"))
              else
                acc.copy(canBeOverloaded = true)
            }
            Right(retval)
          }
    }


    def prepareAsyncShiftedMethodCall(originMethod:Symbol, obj:Tree, nObj: Tree, methods: Seq[Symbol], targs: List[Tree]): ShiftedFun = {
         val canBeOverloaded = methods.nonEmpty && methods.tail.nonEmpty
         val candidates = methods.foldLeft(List.empty[ShiftedFun]) { (candidates, candidateMethod) =>
           checkAsyncShiftedMethod(originMethod, candidateMethod) match
             case Left(err) =>
               // TODO: save error to trace.
               candidates
             case Right(shape) =>
               val nSelect = nObj.select(candidateMethod)
               val fType = summon[CpsTopLevelContext].monadType
               val args0 = Some(List(obj, tctx.cpsMonadRef))
               val nTargs = shape.tp match
                 case ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST =>
                   List(TypeTree(fType.widen))
                 case ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM =>
                   TypeTree(fType) :: targs
                 case ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS =>
                   targs
               val needsInline =  candidateMethod.denot.is(Flags.Inline)
               val nFun = ShiftedFun(fun, nObj, candidateMethod.name.toTermName, nTargs, args0, canBeOverloaded, needsInline, ShiftedArgumentsShape.same)
               nFun :: candidates
         }
         candidates match
           case Nil =>
             throw CpsTransformException(s"Can't find async shifted method ${originMethod.name} for ${obj.tpe.widen.show}", fun.srcPos)
           case other =>
             aggregate(candidates) match
               case Left(err) =>
                 throw CpsTransformException(s"Can't find async shifted method ${originMethod.name} for ${obj.tpe.widen.show}: ${err}", fun.srcPos)
               case Right(retval) =>
                 retval
    }


    /**
     * retrieve shifted method with alreat prepared type-arguments and extra argument list if needed.
     * @param obj
     * @param methodName
     * @param targs
     * @return
     */
    def retrieveShiftedMethod(funSym: Symbol, obj: Tree, methodName: Name, targs:List[Tree] ): Either[String, ShiftedFun] = {


      tryFindInplaceAsyncShiftedMethods(funSym, obj.tpe.widen.classSymbol, methodName, Set("_async","Async","$cps")) match
        case Left(inPlaceErrors) =>
          //TODO: debug output
          resolveAsyncShiftedObject(obj) match
            case Right(nObj) =>
              // TODO:
              // 1. check that method exists
              //   TODO: we should go throught all list.
              val methods = nObj.tpe.allMembers.filter(_.symbol.isMethod).map(_.symbol).filter(_.name == methodName)
              if (methods.isEmpty) then
                report.error(s"Can't find async-shifted method ${methodName} in ${nObj.show}", fun.srcPos)
                report.error(s"all method names: ${nObj.tpe.allMembers.map(_.symbol.name).mkString(",")}", fun.srcPos)
                report.error(s"inplace search: ${inPlaceErrors}", fun.srcPos)
                report.error(s"nObj.tpe=${nObj.tpe.show},  neeedInlining = ${ctx.compilationUnit.needsInlining}")
                throw CpsTransformException(s"Can't find async-shifted method ${methodName} in ${nObj.show}", fun.srcPos)
              // TODO: collect previous errors to pass as parameter
              val mbInlinedObj = maybeInlineObject(nObj)
              val retval = prepareAsyncShiftedMethodCall(fun.symbol, obj, mbInlinedObj, methods, targs)
              Right(retval)
            case Left(err1) =>
              val msg =
                s"""
                 |Can't find async-shifted method or implicit AsyncShift for ${obj.show}
                 |method search: $inPlaceErrors
                 |implicit AsyncShift  object search: $err1
                 """.stripMargin('|')
              Left(msg)
        case Right(methodsWithShape) =>
          // Not,
          println(s"methodsWithShape = ${methodsWithShape}")
          val candidates = methodsWithShape.foldLeft(List.empty[ShiftedFun]) { (candidates, msh) =>
            val (candidateMethod, shape) = msh
            //val nSelect = obj.select(candidateMethod)
            val fType = summon[CpsTopLevelContext].monadType
            val nTargs = shape.tp match
              case ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS =>
                targs
              case ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM =>
                TypeTree(fType) :: targs
              case ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST =>
                if (targs.isEmpty) then
                  List(TypeTree(fType.widen))
                else
                  // impossible ?
                  targs :+ TypeTree(fType.widen)
            val args0 = shape.p match
              case ShiftedArgumentsPlainParamsShape.SAME_PARAMS =>
                None
              case ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST =>
                // one param with monad, because this is in-place substitution
                Some(List(tctx.cpsMonadRef))
              case ShiftedArgumentsPlainParamsShape.EXTRA_FIRST_PARAM =>
                None
            val remainingShapeChange = shape.p match
              case ShiftedArgumentsPlainParamsShape.EXTRA_FIRST_PARAM =>
                ShiftedArgumentsShape(SAME_TYPEPARAMS, EXTRA_FIRST_PARAM)
              case _ =>
                ShiftedArgumentsShape.same
            val needsInlining = candidateMethod.denot.is(Flags.Inline)
            val canBeOverloaded = obj.tpe.member(candidateMethod.name).isOverloaded
            val nFun = ShiftedFun(fun, obj, candidateMethod.name.toTermName, nTargs, args0, canBeOverloaded, needsInlining, remainingShapeChange)
            nFun :: candidates
          }
          aggregate(candidates)
    }

    val shiftedFun = fun match
      case TypeApply(Select(obj,methodName),targs) =>
        retrieveShiftedMethod(fun.symbol, obj,methodName,targs)
      case Select(obj,methodName) =>
        retrieveShiftedMethod(fun.symbol, obj,methodName,Nil)
      case TypeApply(fun@Ident(name),targs) =>
        // this can be method of the enclosing class or method of one of imported projects
        println(s"fun.symbol=${fun.symbol}, fun.tpe=${fun.tpe}")
        if (fun.symbol.owner.isClass) then
          if (fun.symbol.owner == owner) then
            // method of the enclosing class
            println("this is method of the enclosing class")
            retrieveShiftedMethod(fun.symbol, This(owner.asClass),name,targs)
          else
            summon[Context].outersIterator.find(_.owner == fun.symbol.owner) match
              case Some(enclosing) =>
                println("this is method of the indierct enclosing class")
                println(s"find symbol for this:  ${This(enclosing.owner.asClass).symbol}")
                println(s"all methods = ${enclosing.owner.asClass.info.decls.toList.map(_.show)}")
                println(s"")
                retrieveShiftedMethod(fun.symbol, This(enclosing.owner.asClass),name,targs)
              case None =>
                // TODO: check for imported methods
                Left(s"Can't find enclosing class for ${fun.show}")
        else
          Left(s"Can't find async-shifted method for ${fun.show}, unsupported fun tree ${fun}")
      case _ =>
        Left(s"Can't find async-shifted method for ${fun.show}, unsupported fun tree ${fun}")

    shiftedFun

  }



  /*
  def extractFinalResultType(funType:Type, fun:Tree, argss: List[ApplyArgList])(using Context): Type = {
    argss match
      case Nil => funType
      case head :: tail =>
        if (head.isTypeParams) then
          extractFinalResultType(funType, fun, tail)
        else
          funType match
            case mt: MethodOrPoly =>
              extractFinalResultType(mt.resType, fun, tail)
            case AppliedType(tycon, targs) =>
              if (defn.isFunctionType(funType)) then
                extractFinalResultType(targs.last, fun, tail)
              else if (defn.isContextFunctionType(funType)) then
                extractFinalResultType(targs.last, fun, tail)
              //else if (defn.isErasedFunctionType(funType)) then
              //  extractFinalResultType(targs.last, fun, tail)
              else
                throw CpsTransformException(s"Can't extract final result type from ${funType.show}", fun.srcPos)
            case _ =>
              throw CpsTransformException(s"Can't extract final result type from ${funType.show}, expect MethodOrPoly or AppliedType", fun.srcPos)
  }

   */

  def resolveAsyncShiftedObject(obj: Tree)(using Context): Either[String, Tree] = {
    val asyncShift = ref(requiredClass("cps.AsyncShift")).tpe
    //val tpe = AppliedType(asyncShift, List(obj.tpe.widen))
    val tpe = asyncShift.appliedTo(obj.tpe.widen)
    val searchResult = ctx.typer.inferImplicitArg(tpe, obj.span)
    //val searchResult = ctx.typer.implicitArgTree(tpe, fun.span)
    searchResult.tpe match
      case failure: typer.Implicits.SearchFailureType => Left(s"search ${tpe.show} fail :${failure.explanation}")
      case success => Right(searchResult)
  }

  def maybeInlineObject(obj:Tree)(using Context, CpsTopLevelContext): Tree = {
        val isInlined = obj.symbol.denot.is(Flags.Inline)
        if (isInlined) then
          if (summon[CpsTopLevelContext].isBeforeInliner) then
            ctx.compilationUnit.needsInlining=true
            obj
          else
            atPhase(inliningPhase) {
              Inlines.inlineCall(obj)
            }
        else
          obj
  }

  private def wrapInInlined(enclosingInlined: Seq[Inlined], cpsTree: CpsTree)(using Context, CpsTopLevelContext):CpsTree = {
     cpsTree.asyncKind match
       case AsyncKind.Sync =>
         CpsTree.pure(wrapTreeInInlined(enclosingInlined, cpsTree.unpure.get), cpsTree.owner, cpsTree.unpure.get)
       case AsyncKind.Async(internalKind) =>
         CpsTree.impure(wrapTreeInInlined(enclosingInlined, cpsTree.unpure.get), cpsTree.owner, cpsTree.unpure.get, internalKind)
       case AsyncKind.AsyncLambda(bodyKind) =>
         cpsTree match
           case LambdaCpsTree(origin, owner, originDefDef, closureType, cpsBody) =>
             val newCpsBody = wrapInInlined(enclosingInlined, cpsBody)
             LambdaCpsTree(origin, owner, originDefDef, closureType, newCpsBody)
           case BlockBoundsCpsTree(internal) =>
             BlockBoundsCpsTree(wrapInInlined(enclosingInlined, internal))
           case _ =>
             // TODO:check for unpure existence
              CpsTree.opaqueAsyncLambda(cpsTree.origin, cpsTree.owner, wrapTreeInInlined(enclosingInlined, cpsTree.transformed), bodyKind)
  }

  private def wrapTreeInInlined(enclosingInlined: Seq[Inlined], tree: Tree)(using Context): Tree = {
    enclosingInlined.foldLeft(tree){ (s,e) =>
      Inlined(e.call, e.bindings, s).withSpan(e.span)
    }
  }


}
