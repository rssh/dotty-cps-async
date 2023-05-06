package cps.plugin.forest

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Decorators.*
import core.Names.*
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


  def apply(term: Apply, oldOwner: Symbol, newOwner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      Log.trace(s"Apply: origin=${term.show}", nesting)
      val cpsTree = term match
        case Apply(Apply(TypeApply(fCpsAwaitCn,List(tf,ta,tg)),List(fa)), List(gc,gcn)) =>
             println(s"cpsAwait form at : ${term.show},  symbol=${fCpsAwaitCn.symbol}")
             if fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.cpsAwait") ||
                fCpsAwaitCn.symbol == Symbols.requiredMethod("cps.await")        then
                //def cpsAwait[F[_], A, G[_]](fa: F[A])(using CpsMonadContext[G], CpsMonadConversion[F, G]): A =
                Log.info(s"cpsAwait: ${term.show}", nesting,term.srcPos)
                AwaitTransform.fromApply(term, oldOwner, newOwner, nesting, tf, ta, tg, fa, gc, gcn)
             else
               applyMArgs(term, oldOwner, newOwner, nesting, Nil)
        case _ =>
            Log.trace(s"ApplyTransform:default=${term.show}", nesting)
            if (summon[CpsTopLevelContext].isBeforeInliner && atPhase(inliningPhase)(Inlines.needsInlining(term))) {
              // we should inline themself, because in inlined pickkle annotation we have non-cpsed code,
              //  which will be substituted by inliner without cps.
              //println(s"Inlines:needsInlining ${term.show}")
              val inlined = atPhase(inliningPhase)(Inlines.inlineCall(term))
              RootTransform(inlined, oldOwner, newOwner, nesting)
            }else {
              applyMArgs(term, oldOwner, newOwner, nesting, Nil)
            }
      //Log.trace(s" Apply result: ${cpsTree}", nesting)
      cpsTree
  }


  def applyMArgs(term: Apply, oldOwner: Symbol, newOwner: Symbol, nesting:Int, tail:List[ApplyArgList] )(using Context, CpsTopLevelContext): CpsTree = {
    val argList = makeArgList(term, MethodParamsDescriptor(term.fun), oldOwner, newOwner, nesting)
    val retval = term.fun match
      case tfa@Apply(fun1,args1) => 
        applyMArgs(tfa, oldOwner, newOwner, nesting, argList::tail)
      case tpfa@TypeApply(tapp:Apply, targs1) =>
        val targs = makeTypeArgList(tpfa)
        applyMArgs(tapp, oldOwner, newOwner, nesting, targs::argList::tail)
      case _ =>
        parseApplication(term, oldOwner, newOwner, nesting, argList::tail)
    retval
  }


  def parseApplication(appTerm: Apply, oldOwner: Symbol, newOwner: Symbol, nesting:Int, argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
    val cpsApplicant = RootTransform(appTerm.fun, oldOwner,  newOwner, nesting+1 )
    cpsApplicant.unpure match
      case Some(syncFun) =>
        parseSyncFunApplication(appTerm, oldOwner, newOwner, nesting, syncFun, argss)
      case None =>
        val valDefSym = newSymbol(newOwner, "xApplyFun".toTermName, Flags.EmptyFlags,
                        cpsApplicant.originType.widen, Symbols.NoSymbol)
        val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
        val valRef = ref(valDefSym)
        val appCpsTree = parseSyncFunApplication(appTerm, oldOwner, newOwner, nesting, valRef, argss)
        appCpsTree.unpure match
          case Some(syncAppCps) =>
            MapCpsTree(appTerm,newOwner,cpsApplicant,MapCpsTreeArgument(Some(valDef), appCpsTree))
          case None =>
            FlatMapCpsTree(appTerm,newOwner,cpsApplicant,FlatMapCpsTreeArgument(Some(valDef), appCpsTree))
  }


  def parseSyncFunApplication(origin: Apply, oldOwner: Symbol, newOwner:Symbol, nesting: Int, fun: Tree, argss:List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
      val tctx = summon[CpsTopLevelContext]
      val containsAsyncLambda = argss.exists(_.containsAsyncLambda)
      val containsAsync = argss.exists(_.isAsync)
      val retval = if (containsAsyncLambda) {
        tctx.optRuntimeAwait match
          case Some(runtimeAwait) =>
            genApplication(origin,oldOwner,newOwner,MbShiftedFun(fun,false),argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC,Some(runtimeAwait)), false)
          case None =>
            if (fun.denot != NoDenotation) {
                  // check -- can we add shifted version of fun
                  val newFun = retrieveShiftedFun(origin,fun,oldOwner, newOwner)
                  val r = genApplication(origin, oldOwner, newOwner, newFun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC_SHIFT, None), true)

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
        genApplication(origin, oldOwner, newOwner, MbShiftedFun(fun,false), argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, None), false)
      } else {
        parseSyncFunPureApplication(origin, oldOwner, newOwner, fun, argss)
      }
      retval
  }

  //  just unchanged
  def parseSyncFunPureApplication(origin: Apply, oldOwner: Symbol, newOwner: Symbol, fun: Tree, argss:List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
     val plainTree = argss.foldLeft(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Apply(s,args.map(_.exprInCall(ApplyArgCallMode.SYNC,None))).withSpan(orig.span)
     }
     if (argss.exists(_.containsMonadContext)) {
       println(s"adoptCpsedCall1: plainTree=${plainTree.show}")
       val adoptedTree = Scaffolding.adoptCpsedCall(plainTree, plainTree.tpe.widen, summon[CpsTopLevelContext].monadType)
       println(s"adoptCpsedCall1: adoptedTree=${adoptedTree.show}")
       CpsTree.impure(origin, newOwner, adoptedTree.changeOwner(oldOwner, newOwner), AsyncKind.Sync)
     } else {
       CpsTree.pure(origin, newOwner, plainTree.changeOwner(oldOwner, newOwner))
     }
  }

  def genApplication(origin:Apply, oldOwner: Symbol, newOwner: Symbol, fun: MbShiftedFun, argss: List[ApplyArgList], f: ApplyArg => Tree, isImpure: Boolean)(using Context, CpsTopLevelContext): CpsTree = {
    println(s"genApplication origin: ${origin.show}")

    def genOneLastPureApply(fun: Tree, argList: ApplyArgList): Tree = {
      val tree = argList match
        case ApplyTypeArgList(origin, targs) =>
          TypeApply(fun, targs).withSpan(origin.span)
        case ApplyTermArgList(origin, args) =>
          Apply(fun, args.map(f)).withSpan(origin.span)
      tree.changeOwner(oldOwner, newOwner)
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
              origin.changeOwner(oldOwner, newOwner),
              newOwner,
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
        println(s"pureReplyInlinedIfNeeded: before inlined=${pureReply.show}")
        println(s"pureReplyInlinedIfNeeded: inlined=${inlined.show}")
        println(s"pureReplyInlinedIfNeeded: inlined tree =${inlined}")
        println(s"pureReplyInlinedIfNeeded: inlined.tpe=${inlined.tpe}")
        inlined
    } else {
        pureReply
    }
    val lastCpsTree = if (argss.exists(_.containsMonadContext)) {
      val adoptedPureReply = Scaffolding.adoptCpsedCall(pureReply, origin.tpe.widen, summon[CpsTopLevelContext].monadType)
      println("!!!adoptedCpsedCall-2: ${adoptedPureReply.show}")
      CpsTree.impure(origin, newOwner, adoptedPureReply, AsyncKind.Sync)
    } else if (isImpure) {
      CpsTree.impure(origin, newOwner, pureReply, AsyncKind.Sync)
    } else {
      CpsTree.pure(origin,newOwner,pureReply)
    }

    val nApplyCpsTree = genPrefixes(argss, lastCpsTree)
    val retval = nApplyCpsTree
    println(s"genApplication result: ${retval.show}")
    println(s"genApplication exists containsMonadContext: ${argss.exists(_.containsMonadContext)}")
    retval
  }




  def makeArgList(term: Apply, mt: MethodParamsDescriptor, oldOwner: Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): ApplyTermArgList = {
    // need to calculate dependency between arguments.
    ApplyTermArgList.make(term, mt, oldOwner: Symbol, newOwner: Symbol, nesting: Int)
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
  def retrieveShiftedFun(origin: Tree,  fun:Tree, oldOwner:Symbol, newOwner: Symbol)(using Context, CpsTopLevelContext): MbShiftedFun = {

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