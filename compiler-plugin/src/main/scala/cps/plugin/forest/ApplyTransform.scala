package cps.plugin.forest

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.StdNames.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span
import core.Types.*
import core.Phases.*
import cps.plugin.*
import cps.plugin.forest.application.*
import QuoteLikeAPI.*
import cps.{CpsMonadContext, CpsMonadConversion}
import inlines.Inlines
import transform.Inlining


object ApplyTransform {

  case class MbShiftedFun(
                           tree: Tree,
                           callShouldBeInlined: Boolean,
                         )

  case class FunCallMode(
                          funKind: AsyncKind,
                          preliminaryResultKind: AsyncKind,
                          asyncLambdaApplication: Boolean
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
        case Apply(cnThrow, List(_)) if (cnThrow.symbol == defn.throwMethod) =>
             ThrowTransform(term, owner, nesting)
        case _ =>
            if (summon[CpsTopLevelContext].isBeforeInliner && atPhase(inliningPhase)(Inlines.needsInlining(term))) {
              // we should inline themself, because in inlined pickkle annotation we have non-cpsed code,
              //  which will be substituted by inliner without cps.
              //println(s"Inlines:needsInlining ${term.show}")
              val inlined = atPhase(inliningPhase)(Inlines.inlineCall(term))
              RootTransform(inlined, owner, nesting)
            }else {
              applyMArgs(term, owner, nesting, Nil)
            }
      Log.trace(s" Apply result: ${cpsTree.show}", nesting)
      Log.trace(s" Apply result transformed: ${cpsTree.transformed.show}", nesting)
      cpsTree
  }


  def applyMArgs(term: Apply, owner: Symbol, nesting:Int, tail:List[ApplyArgList] )(using Context, CpsTopLevelContext): CpsTree = {
    val argList = makeArgList(term, MethodParamsDescriptor(term.fun), owner, nesting)
    val retval = term.fun match
      case tfa@Apply(fun1,args1) =>
        applyMArgs(tfa, owner, nesting, argList::tail)
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
      case sel@Select(obj,method) =>
        val cpsObj = RootTransform(obj,owner,nesting+1)
        cpsObj.asyncKind match {
          case AsyncKind.Sync =>
            val syncFun = cpsObj.select(sel)
            val callMode = FunCallMode(AsyncKind.Sync, AsyncKind.Sync, false)
            val cpsTree = parseSyncFunApplication(appTerm, owner, nesting, syncFun.unpure.get, argss, callMode)
            cpsTree
          case AsyncKind.Async(internalKind) =>
            val syncFun = cpsObj.select(sel)
            val callMode = FunCallMode(syncFun.asyncKind, AsyncKind.Sync,  false)
            parseApplicationCpsFun(appTerm, owner, nesting, syncFun, argss, callMode)
          case AsyncKind.AsyncLambda(bodyKind) =>
            if (method == nme.apply) {
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
                  val callMode = FunCallMode(AsyncKind.Sync, bodyKind, true)
                  parseSyncFunApplication(appTerm, owner, nesting, nFun, argss, callMode)
                case AsyncKind.AsyncLambda(bodyKind2) =>
                  throw CpsTransformException("Shape (async labda which returns async lambda) is notsupported yet", appTerm.srcPos)
              }
            } else {
              //  TODO:  implement andThen .. etc
              throw CpsTransformException("Only apply is supported for async lambda now", appTerm.srcPos)
            }
        }
      case _ =>
        parseApplicationNonLambda(appTerm, owner, nesting, argss)
    }
  }

  def parseApplicationNonLambda(appTerm: Apply, owner: Symbol, nesting:Int, argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
    val cpsApplicant = RootTransform(appTerm.fun, owner, nesting+1 )
    Log.trace(s"ApplyTransfopm.parseApplicationNonLambda  cpsApplicant=: ${cpsApplicant.show}", nesting)
    Log.trace(s"ApplyTransfopm.parseApplicationNonLambda  cpsApplicant.asyncKind=: ${cpsApplicant.asyncKind}", nesting)
    val callMode = FunCallMode(cpsApplicant.asyncKind, AsyncKind.Sync, false)
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


  def parseSyncFunApplication(origin: Apply, owner:Symbol, nesting: Int, fun: Tree, argss:List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
      val tctx = summon[CpsTopLevelContext]
      val containsAsyncLambda = argss.exists(_.containsAsyncLambda)
      val containsAsync = argss.exists(_.isAsync)
      val retval = if (containsAsyncLambda) {
        tctx.optRuntimeAwait match
          case Some(runtimeAwait) =>
            genApplication(origin,owner,MbShiftedFun(fun,false),argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC,Some(runtimeAwait)), callMode)
          case None =>
            if (fun.denot != NoDenotation) {
                  // check -- can we add shifted version of fun
                  val newFun = retrieveShiftedFun(origin,fun,owner)
                  val newFunType = newFun.tree.tpe.widen
                  // TOOD: anylize newFunType and deduce preliminaryReturnType from it.
                  println(s"shiftedFunType: ${newFunType.show}")
                  val callMode = FunCallMode(AsyncKind.Sync, AsyncKind.Async(AsyncKind.Sync), false)
                  val r = genApplication(origin, owner, newFun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC_SHIFT, None), callMode)
                  println(s"application of shifted function: ${r.show},  newFun=${newFun.tree.show}")
                  println(s"origin=${origin.show}")
                  println(s"argss=${argss.map(_.show).mkString(",")}")
                  r
            } else {
              fun match
                case QuoteLikeAPI.CheckLambda(params,body,bodyOwner) =>
                  // Lambda transdorm
                  ???
                case _ =>
                  throw CpsTransformException(s"Can't transform function ${fun}",fun.srcPos)
            }
      } else if (containsAsync) {
        genApplication(origin, owner, MbShiftedFun(fun,false), argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, None), callMode)
      } else {
        parseSyncFunPureApplication(origin, owner, fun, argss, callMode)
      }
      retval
  }

  //  just unchanged
  // TODO: pass internal async-kine in impure parameter
  def parseSyncFunPureApplication(origin: Apply, owner: Symbol, fun: Tree, argss:List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
     println(s"parseSyncFunPureApplication: fun=${fun.show}")
     val plainTree = argss.foldLeft(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Apply(s,args.map(_.exprInCall(ApplyArgCallMode.SYNC,None))).withSpan(orig.span)
     }
     val fullOrigin = if (argss.isEmpty) origin else argss.last.origin
     adoptCallMode(fullOrigin, plainTree, owner, argss, callMode)
  }

  def adoptCallMode(origin: Tree, plainTree: Tree, owner: Symbol, argss: List[ApplyArgList], callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
    if (argss.exists(_.containsDirectContext) ) {
      val adoptedTree = if (!callMode.asyncLambdaApplication) {
         Scaffolding.adoptCpsedCall(plainTree, plainTree.tpe.widen, summon[CpsTopLevelContext].monadType)
      } else plainTree
      //if (isImpure) {
      //  TODO: such situationis possible when we pass lamba with context parameters (can be inline)
      //  TODO:  separate this case.
      //  throw CpsTransformException(s"Impure call in combination of callign context functon is not supported yet", origin.srcPos)
      //}
      CpsTree.impure(origin, owner, adoptedTree, callMode.preliminaryResultKind)
    } else {
      adoptResultKind(origin, plainTree, owner, callMode.preliminaryResultKind)
    }
  }

  def adoptResultKind(origin:Tree, newApply: Tree, owner: Symbol, resultKind: AsyncKind): CpsTree = {
    resultKind match
      case AsyncKind.Sync => PureCpsTree(origin, owner, newApply)
      case AsyncKind.Async(internalKind) => CpsTree.impure(origin, owner, newApply, internalKind)
      case AsyncKind.AsyncLambda(body) =>
             ???
             //  TODO:  write OpaqueAsyncFunction  instead InlineCpsTree with kind: AsyncLambda
  }

  def genApplication(origin:Apply, owner: Symbol, fun: MbShiftedFun, argss: List[ApplyArgList], f: ApplyArg => Tree, callMode: FunCallMode)(using Context, CpsTopLevelContext): CpsTree = {
    println(s"genApplication origin: ${origin.show}")

    def genOneLastPureApply(fun: Tree, argList: ApplyArgList): Tree = {
      val tree = argList match
        case ApplyTypeArgList(origin, targs) =>
          TypeApply(fun, targs).withSpan(origin.span)
        case ApplyTermArgList(origin, args) =>
          Apply(fun, args.map(f)).withSpan(origin.span)
      tree
    }

    @tailrec
    def genPureReply(fun:Tree, argss: List[ApplyArgList]): Tree =
      argss match
        case Nil => fun
        case head::tail => genPureReply(genOneLastPureApply(fun, head),tail)

    def genOneApplyPrefix(origin: Tree, args:List[ApplyArg], tailCpsTree:CpsTree): CpsTree =
        args.foldRight(tailCpsTree) { (e,s) =>
          e.flatMapsBeforeCall.foldRight(s){ (pre ,tail) =>
            val (prefixCpsTree, prefixVal) = pre
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
        }

    def genPrefixes(argss:List[ApplyArgList], tailCpsTree:CpsTree): CpsTree =
      argss.foldRight(tailCpsTree) { (e,s) =>
         e match
          case ApplyTermArgList(origin,args) =>
            genOneApplyPrefix(origin,args,s)
          case _ => s
      }

    val pureReply = genPureReply(fun.tree,argss)
    println(s"pureReply symbol = ${pureReply.symbol}")
    val pureReplyMbInlined = if (fun.callShouldBeInlined && summon[CpsTopLevelContext].isAfterInliner) {
        val inlined = atPhase(inliningPhase){
           Inlines.inlineCall(pureReply)
        }
        inlined
    } else {
        pureReply
    }
    val fullOrigin = if (argss.isEmpty) origin else argss.last.origin
    val lastCpsTree = adoptCallMode(fullOrigin, pureReply, owner, argss, callMode)
    val nApplyCpsTree = genPrefixes(argss, lastCpsTree)
    val retval = nApplyCpsTree
    println(s"genApplication result: ${retval.show}")
    println(s"genApplication exists containsMonadContext: ${argss.exists(_.containsDirectContext)}")
    retval
  }




  def makeArgList(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): ApplyTermArgList = {
    // need to calculate dependency between arguments.
    ApplyTermArgList.make(term, mt, owner: Symbol, nesting: Int)
  }


  def makeTypeArgList(term: TypeApply)(using Context): ApplyTypeArgList = {
    ApplyTypeArgList(term,term.args.map(tt => TypeTree(tt.tpe)))
  }


  /**
   * retrieve shifted function or throw exception.
   * @param fun
   * @param owner
   * @param Context
   * @param CpsTopLevelContext
   * @return  new function (with type arguments and additional parameter list if needed)
   */
  def retrieveShiftedFun(origin: Tree,  fun:Tree, owner: Symbol)(using Context, CpsTopLevelContext): MbShiftedFun = {

    val tctx = summon[CpsTopLevelContext]

    def matchInplaceArgTypes(originSym:Symbol, candidateSym: Symbol): Either[String,ShiftedArgumentsInplaceShape] = {
      val originParamSymms = originSym.paramSymss
      val candidateParamSymms = candidateSym.paramSymss
      if (candidateParamSymms.isEmpty) then
        Left(s"${candidateSym.name} have no arguments")
      else if (originParamSymms.length == candidateParamSymms.length) then
        val originTpArgs = originParamSymms.head.filter(_.isType)
        val candidateTpArgs = candidateParamSymms.head.filter(_.isType)
        if (!originTpArgs.isEmpty) then
            if (candidateTpArgs.length == originTpArgs.length+1) then
              Right(ShiftedArgumentsInplaceShape.EXTRA_TYPEPARAM)   //  with F[_]  in type-args
            else if (candidateTpArgs.length == originTpArgs.length) then
              Right(ShiftedArgumentsInplaceShape.SAME_PARAMS)
            else
              Left(s"Can't match parameters in ${originSym} and ${candidateSym}")
        else if (candidateTpArgs.isEmpty) then
          Right(ShiftedArgumentsInplaceShape.SAME_PARAMS)
        else
          Left(s"Can't match parameters in ${originSym} and ${candidateSym} - first have type args when second - not")
      else if (originParamSymms.length + 1 == candidateParamSymms.length) then
        val candidateTpArgs = candidateParamSymms.head.filter(_.isType)
        if (candidateTpArgs.isEmpty) then
          Left(s"Can't match parameters in ${originSym} and ${candidateSym} - added arglis shoule be type")
        else
          Right(ShiftedArgumentsInplaceShape.EXTRA_TYPEPARAM_LIST)
      else
        Left(s"Can't handle difference in number of parameters between ${originSym} and ${candidateSym}")
    }



    def tryFindInplaceAsyncShiftedMethods(objSym: Symbol, name: Name, suffixes: Set[String]): Either[String,Map[Symbol,ShiftedArgumentsInplaceShape]]  = {
      val shapes = (for{ m <- objSym.lookupPrefix.allMembers
                suffix <- suffixes if m.symbol.isMethod && m.symbol.name.toString == name.toString + suffix
                matchShape = matchInplaceArgTypes(fun.symbol, objSym)
      } yield (m.symbol, matchShape)).toMap
      val (pos, neg) = shapes.partition(_._2.isRight)
      if (pos.isEmpty) then
        Left( neg.values.map(_.left).mkString("\n") )
      else
        Right( pos.mapValues(_.right.get).toMap )
    }


    def resolveAsyncShiftedObject(obj: Tree): Either[String,Tree] = {
      val asyncShift = ref(requiredClass("cps.AsyncShift")).tpe
      val tpe = AppliedType(asyncShift, List(obj.tpe.widen))
      val searchResult = ctx.typer.inferImplicitArg(tpe, fun.span)
      searchResult.tpe match
        case failure : typer.Implicits.SearchFailureType => Left(failure.explanation)
        case success => Right(searchResult)
    }

    def checkAsyncShiftedMethod(originMethod: Symbol, candidateMethod: SymDenotation): Either[String,ShiftedArgumentsShiftedObjectShape] = {
      val originTpArgs = originMethod.paramSymss.head.filter(_.isType)
      if (originTpArgs.isEmpty) then
        if (candidateMethod.paramSymss.length == originMethod.paramSymss.length + 2)
          //  with extra type-arg and arglist wich pass monad
          Right(ShiftedArgumentsShiftedObjectShape.EXTRA_TYPEPARAM_LIST)
        else
          Left(s"Can't match parameters in ${originMethod} and ${candidateMethod}")
      else if (candidateMethod.paramSymss.length == originMethod.paramSymss.length + 1)
        //  with extra type-arg and arglist wich pass monad
        Right(ShiftedArgumentsShiftedObjectShape.EXTRA_TYPEPARAM)
      else
        Left(s"Can't match parameters in ${originMethod} and ${candidateMethod}")
    }


    @tailrec
    def prepareAsyncShiftedMethodCall(originMethod:Symbol, obj:Tree, nObj: Tree, methods: Seq[Symbol], targs: List[Tree]): Tree = {
       if (methods.isEmpty)
         throw CpsTransformException(s"Can't find async-shifted method ${originMethod.name} of ${obj.show} in ${nObj.show}", fun.srcPos)
       else {
         val candidateMethod = methods.head
         checkAsyncShiftedMethod(originMethod, candidateMethod) match
           case Left(err) => prepareAsyncShiftedMethodCall(originMethod, obj, nObj, methods.tail, targs)
           case Right(shape) =>
             val nSelect = nObj.select(candidateMethod)
             val fType = summon[CpsTopLevelContext].monadType
             shape match
               case ShiftedArgumentsShiftedObjectShape.EXTRA_TYPEPARAM_LIST =>
                   Apply(
                     TypeApply(nSelect, List(TypeTree(fType))),
                     List(obj, tctx.cpsMonadRef)
                   ).withSpan(fun.span)
               case ShiftedArgumentsShiftedObjectShape.EXTRA_TYPEPARAM =>
                   Apply(
                     TypeApply(nSelect, TypeTree(fType)::targs),
                     List(obj, tctx.cpsMonadRef)
                   ).withSpan(fun.span)
       }
    }

    def retrieveShiftedMethod(obj: Tree, methodName: Name, targs:List[Tree] ): MbShiftedFun = {

      tryFindInplaceAsyncShiftedMethods(obj.tpe.widen.classSymbol, methodName, Set("_async","Async","$cps")) match
        case Left(err) =>
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
                report.error(s"nObj.tpe=${nObj.tpe.show},  neeedInlining = ${ctx.compilationUnit.needsInlining}")
                throw CpsTransformException(s"Can't find async-shifted method ${methodName} in ${nObj.show}", fun.srcPos)
              // TODO: collect previous errors to pass as parameter
              println(s"find shiftedObject,  nObj=${nObj.show}")
              println(s"shiftedObject tree,  nObj=${nObj}")
              val isInlined = nObj.symbol.denot.is(Flags.Inline)
              val mbInlinedObj = if (isInlined) {
                if (tctx.isBeforeInliner) then
                  ctx.compilationUnit.needsInlining=true
                  nObj
                else
                  atPhase(inliningPhase) {
                    Inlines.inlineCall(nObj)
                  }
              } else {
                println(s"shiftedObject is not inline")
                nObj
              }
              val tree = prepareAsyncShiftedMethodCall(fun.symbol, obj, mbInlinedObj, methods, targs)
              MbShiftedFun(tree,false)
            case Left(err1) =>
              report.error("Can't find async-shifted method or implicit AsyncShift for "+obj.show, fun.srcPos)
              report.error(s" method search: $err", fun.srcPos)
              report.error(s" implicit AsyncShifg object search: $err1", fun.srcPos)
              throw CpsTransformException("Cn't find async-shifted method or implicit AsyncShift for "+obj.show, fun.srcPos)
        case Right(methodsWithShape) =>
          methodsWithShape.headOption match
            case Some((sym,shape)) =>
               // TODO: we should check nArgs, because now typeAppl
              val isInlined = sym.denot.is(Flags.Inline)
              if (isInlined && tctx.isBeforeInliner) then
                ctx.compilationUnit.needsInlining=true
              val funWithoutTypeapply = Select(obj,TermRef(obj.tpe,sym)).withSpan(fun.span)
              val tree = shape match
                case ShiftedArgumentsInplaceShape.SAME_PARAMS =>
                  funWithoutTypeapply
                case ShiftedArgumentsInplaceShape.EXTRA_TYPEPARAM =>
                  val tctx = summon[CpsTopLevelContext]
                  val retval = TypeApply(funWithoutTypeapply, TypeTree(tctx.monadType) :: targs).withSpan(fun.span)
                  retval
                case ShiftedArgumentsInplaceShape.EXTRA_TYPEPARAM_LIST =>
                  TypeApply(funWithoutTypeapply, targs :+ TypeTree(obj.tpe.widen)).withSpan(fun.span)
              MbShiftedFun(tree,isInlined)
            case None =>
              throw CpsTransformException(s"Can't find async-shifted method ${methodName} for ${obj.show}", fun.srcPos)

    }

    val shiftedFun = fun match
      case TypeApply(Select(obj,methodName),targs) =>
        retrieveShiftedMethod(obj,methodName,targs)
      case Select(obj,methodName) =>
        retrieveShiftedMethod(obj,methodName,Nil)
      case _ =>
        ???

    shiftedFun

  }

}