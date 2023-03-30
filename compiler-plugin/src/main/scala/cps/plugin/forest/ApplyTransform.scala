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


import cps.plugin.*
import cps.plugin.forest.application.*
import QuoteLikeAPI.*


object ApplyTransform {

  def apply(term: Apply, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      //Log.trace(s"Apply: origin=${term.show}", nesting)
      val cpsTree = applyMArgs(term,owner, nesting, Nil)
      //Log.trace(s"Apply result: ${cpsTree}", nesting)
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
        parseApplication(term,owner, nesting, argList::tail)
    retval
  }


  def parseApplication(appTerm: Apply, owner: Symbol, nesting:Int, argss: List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
    val cpsApplicant = RootTransform(appTerm.fun ,owner, nesting+1 )
    cpsApplicant.unpure match
      case Some(syncFun) =>
        parseSyncFunApplication(appTerm, owner, nesting, syncFun, argss)
      case None =>
        val valDefSym = newSymbol(owner, "xApplyFun".toTermName, Flags.EmptyFlags, 
                        cpsApplicant.originType.widen, Symbols.NoSymbol)
        val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
        val valRef = ref(valDefSym)
        val appCpsTree = parseSyncFunApplication(appTerm, owner, nesting, valRef, argss)
        appCpsTree.unpure match
          case Some(syncAppCps) =>
            MapCpsTree(appTerm,owner,cpsApplicant,MapCpsTreeArgument(Some(valDef), appCpsTree))
          case None =>
            FlatMapCpsTree(appTerm,owner,cpsApplicant,FlatMapCpsTreeArgument(Some(valDef), appCpsTree))
  }


  def parseSyncFunApplication(origin: Apply, owner: Symbol, nesting: Int, fun: Tree, argss:List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
      val tctx = summon[CpsTopLevelContext]
      val containsAsyncLambda = argss.exists(_.containsAsyncLambda)
      val containsAsync = argss.exists(_.isAsync)
      if (containsAsyncLambda) {
        tctx.optRuntimeAwait match
          case Some(runtimeAwait) =>
            genApplication(origin,owner,fun,argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC,Some(runtimeAwait)))
          case None =>
            if (fun.denot != NoDenotation) {
                  // check -- can we add shifted version of fun
                  // actually, two strategies are possible:
                  //    - 1.  Generate two copies of HO functions for all libs [?]
                  //           problem -- extra code (we can remove unused if we not in library later(?))
                  //                   -- fragmentation of ecosystem: we need to know, if library artifacts 
                  //                   -- contains those function
                  //    - 2.  Generate only when needed, here we can mark that one is needed.
                  //           problem -- it's reverse dependency.  I.e. if we change call-site, that
                  //                      we need to recompile origin
                  //  now we encapsulate this in shiftedFunction cache
                  val changedFun = retrieveShiftedFun(fun,owner)
                  genApplication(origin, owner, changedFun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC_SHIFT, None))
            } else {
              fun match
                case QuoteLikeAPI.CheckLambda(params,body,bodyOwner) =>
                  // Lambda transdorm
                  ???
                case _ =>  
                  throw CpsTransformException(s"Can't transform function ${fun}",fun.srcPos)
            }
      } else if (containsAsync) {
        genApplication(origin, owner, fun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, None))
      } else {
        parseSyncFunPureApplication(origin,owner, fun, argss)
      }
  }

  //  just unchanged
  def parseSyncFunPureApplication(origin: Apply, owner: Symbol, fun: Tree, argss:List[ApplyArgList])(using Context, CpsTopLevelContext): CpsTree = {
     val plainTree = argss.foldLeft(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Apply(s,args.map(_.exprInCall(ApplyArgCallMode.SYNC,None))).withSpan(orig.span)
     }
     if (argss.exists(_.containsMonadContext)) {
       CpsTree.impure(origin, owner, plainTree)
     } else {
       CpsTree.pure(origin, owner, plainTree)
     }
  }

  def genApplication(origin:Apply, owner: Symbol, fun: Tree, argss: List[ApplyArgList], f: ApplyArg => Tree)(using Context, CpsTopLevelContext): CpsTree = {
    println(s"genApplication origin: ${origin.show}")
    
    def genOneLastPureApply(fun: Tree, argList: ApplyArgList): Tree =
      argList match
        case ApplyTypeArgList(origing, targs) =>
          TypeApply(fun,targs).withSpan(origin.span)
        case ApplyTermArgList(origin, args) =>
          val l = Apply(fun, args.map(f)).withSpan(origin.span)
            l
  
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

    val pureReply = genPureReply(fun,argss)    
    val nApplyCpsTree = genPrefixes(argss, CpsTree.pure(origin,owner,pureReply))
    // if one of argument is monadic context, than return type should changed from T to F[T],
    // therefore we should appropriative change Apply
    val retval = if (argss.exists(_.containsMonadContext)) {
      nApplyCpsTree.asyncKind match {
        case AsyncKind.Sync =>
          CpsTree.impure(nApplyCpsTree.origin,nApplyCpsTree.owner,nApplyCpsTree.unpure.get, AsyncKind.Async(AsyncKind.Sync))
        case otherKind =>
          // TODO: prove lambda translation
          // F[T] => F[F[T]], or (A=>Cps[B]) => F[A=>Cps[B])]]   compansate this by flatMap to identity
          val ffType = decorateTypeApplications(summon[CpsTopLevelContext].monadType).appliedTo(nApplyCpsTree.transformedType)
          val nSym = Symbols.newSymbol(owner, "xx".toTermName, Flags.Synthetic, ffType)
          val nValDef = ValDef(nSym,TypeTree(ffType), true).withSpan(origin.span)
          val nRef = ref(nSym).withSpan(origin.span)
          FlatMapCpsTree(nApplyCpsTree.origin,
            nApplyCpsTree.owner,
            nApplyCpsTree,
            FlatMapCpsTreeArgument(Some(nValDef),
              CpsTree.impure(nRef,nApplyCpsTree.owner, nRef, AsyncKind.Async(otherKind))
            )
          )
      }
    } else {
      nApplyCpsTree
    }
    println(s"genApplication result: ${retval.show}")
    retval

  }

  


  def makeArgList(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): ApplyTermArgList = {
    // need to calculate dependency between arguments.
    ApplyTermArgList.make(term, mt, owner, nesting: Int)
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
   * @return
   */
  def retrieveShiftedFun(fun:Tree, owner:Symbol)(using Context, CpsTopLevelContext): Tree = {

    def matchInplaceArgTypes(originSym:Symbol, candidateSym: Symbol): Either[String,ShiftedArgumentsShape] = {
      val originParamSymms = originSym.paramSymss
      val candidateParamSymms = candidateSym.paramSymss
      if (candidateParamSymms.isEmpty) then
        Left(s"${candidateSym.name} have no arguments")
      else if (originParamSymms.length == candidateParamSymms.length) then
        val originTpArgs = originParamSymms.head.filter(_.isType)
        val candidateTpArgs = candidateParamSymms.head.filter(_.isType)
        if (!originTpArgs.isEmpty) then
            if (candidateTpArgs.length == originTpArgs.length+1) then
              Right(ShiftedArgumentsShape.EXTRA_TYPEPARAM)   //  with F[_]  in type-args
            else if (candidateTpArgs.length == originTpArgs.length) then
              Right(ShiftedArgumentsShape.SAME_PARAMS)
            else
              Left(s"Can't match parameters in ${originSym} and ${candidateSym}")
        else if (candidateTpArgs.isEmpty) then
          Right(ShiftedArgumentsShape.SAME_PARAMS)
        else
          Left(s"Can't match parameters in ${originSym} and ${candidateSym} - first have type args when second - not")  
      else if (originParamSymms.length + 1 == candidateParamSymms.length) then
        val candidateTpArgs = candidateParamSymms.head.filter(_.isType)
        if (candidateTpArgs.isEmpty) then
          Left(s"Can't match parameters in ${originSym} and ${candidateSym} - added arglis shoule be type")
        else
          Right(ShiftedArgumentsShape.EXTRA_TYPEPARAM_LIST)
      else
        Left(s"Can't handle difference in number of parameters between ${originSym} and ${candidateSym}")
    }

    def tryFindInplaceAsyncShiftedMethods(objSym: Symbol, name: Name, suffixes: Set[String]): Either[String,Map[Symbol,ShiftedArgumentsShape]]  = {
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

    def retrieveShiftedMethod(obj: Tree, methodName: Name, targs:List[Tree] ): Tree = {

      tryFindInplaceAsyncShiftedMethods(obj.tpe.widen.classSymbol, methodName, Set("_async","Async","$cps")) match
        case Left(err) =>
          //TODO: debug output
          resolveAsyncShiftedObject(nObj) match
            case Right(value) =>
              // TODO:
              // 1. check that method exists
              val method = nObj.typeSymbol.member(methodName) 
              if (!method.exists) then
                throw CpsTransformException(s"Can't find async-shifted method ${methodName} in ${nObj}", fun.span)
              // is we need additional type-args?

              nObj.select(methodName)
              // 2. check that method have same type-args as in origin method
              ???
            case Left(err1) =>
              reporting.error("Can't find async-shifted method or implicit AsyncShift for "+obj.show, fun.span)
              reporting.error(s" method search: $err", fun.span)
              reporting.error(s" implicit AsyncShifg object search: $err1", fun.span)
              throw CpsTransformException("Cn't find async-shifted method or implicit AsyncShift for "+obj.show, fun.span)
        case Right(methodsWithShape) =>
          methodsWithShape.headOption match
            case Some((sym,shape)) =>
              val funWithoutTypeapply =  Select(obj,TermRef(obj.tpe,sym)).withSpan(fun.span)
              shape match
                case ShiftedArgumentsShape.SAME_PARAMS =>
                  funWithoutTypeapply
                case ShiftedArgumentsShape.EXTRA_TYPEPARAM =>
                  TypeApply(funWithoutTypeapply, tctx.monadType :: targs).withSpan(fun.span)
                case ShiftedArgumentsShape.EXTRA_TYPEPARAM_LIST =>
                  TypeApply(funWithoutTypeapply, targs :+ TypeTree(obj.tpe.widen)).withSpan(fun.span)
            case None => EmptyTree

    }

    fun match
      case TypeApply(Select(obj,methodName),targs) =>
        retrieveShiftedMethod(obj,methodName,targs)
      case Select(obj,methodName) =>
        retrieveShiftedMethod(obj,methodName,Nil)
      case _ =>
        ???  
  }

}