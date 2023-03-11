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

  def apply(term: Apply, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
      val cpsTree = applyMArgs(term,owner,tctx, Nil)
      println(s"applyTransform: origin=${term.show}, type=${term.tpe.widen.show}")
      println(s"applyTransform: result=${cpsTree.show}")
      println(s"applyTransform: transformed=${cpsTree.transformed.show}")
      cpsTree
  }


  def applyMArgs(term: Apply, owner: Symbol, tctx: TransformationContext, tail:List[ApplyArgList] )(using Context): CpsTree = {
    val argList = makeArgList(term, MethodParamsDescriptor(term.fun), owner, tctx)
    val retval = term.fun match
      case tfa@Apply(fun1,args1) => 
        applyMArgs(tfa, owner, tctx, argList::tail)
      case tpfa@TypeApply(tapp:Apply, targs1) =>
        val targs = makeTypeArgList(tpfa)
        applyMArgs(tapp, owner, tctx, targs::argList::tail)  
      case _ => 
        parseApplication(term,owner,tctx,argList::tail)
    retval
  }

  def parseApplication(appTerm: Apply, owner: Symbol, tctx: TransformationContext, argss: List[ApplyArgList])(using Context): CpsTree = {
    val cpsApplicant = RootTransform(appTerm.fun ,owner, tctx)
    cpsApplicant.unpure match
      case Some(syncFun) => 
        parseSyncFunApplication(appTerm, owner, tctx, syncFun, argss)
      case None =>
        val valDefSym = newSymbol(owner, "xApplyFun".toTermName, Flags.EmptyFlags, 
                        cpsApplicant.originType.widen, Symbols.NoSymbol)
        val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
        val valRef = ref(valDefSym)
        val appCpsTree = parseSyncFunApplication(appTerm, owner, tctx, valRef, argss)
        appCpsTree.unpure match
          case Some(syncAppCps) =>
            MapCpsTree(tctx,appTerm,owner,cpsApplicant,MapCpsTreeArgument(Some(valDef), appCpsTree))
          case None =>
            FlatMapCpsTree(tctx,appTerm,owner,cpsApplicant,FlatMapCpsTreeArgument(Some(valDef), appCpsTree))
  }



  def parseSyncFunApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, argss:List[ApplyArgList])(using Context): CpsTree = {
      val containsAsyncLambda = argss.exists(_.containsAsyncLambda)
      val containsAsync = argss.exists(_.isAsync)
      if (containsAsyncLambda) {
        tctx.optRuntimeAwait match
          case Some(runtimeAwait) =>
            genApplication(origin,owner,tctx,fun,argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC,Some(runtimeAwait),tctx))
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
                  val changedFun = retrieveShiftedFun(fun,tctx,owner)
                  genApplication(origin, owner, tctx, changedFun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC_SHIFT, None, tctx))
            } else {
              fun match
                case QuoteLikeAPI.CheckLambda(params,body,bodyOwner) =>
                  // Lambda transdorm
                  ???
                case _ =>  
                  throw CpsTransformException(s"Can't transform function ${fun}",fun.srcPos)
            }
      } else if (containsAsync) {
        genApplication(origin, owner, tctx, fun, argss, arg => arg.exprInCall(ApplyArgCallMode.ASYNC, None, tctx))
      } else {
        parseSyncFunPureApplication(origin,owner,tctx, fun, argss)
      }
  }

  //  just unchanged
  def parseSyncFunPureApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, args:List[ApplyArgList])(using Context): CpsTree = {
     val plainTree = args.foldLeft(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Apply(s,args.map(_.exprInCall(ApplyArgCallMode.SYNC,None,tctx))).withSpan(orig.span)
     }
     CpsTree.pure(tctx, origin, owner, plainTree)
  }

  def genApplication(origin:Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, argss: List[ApplyArgList], f: ApplyArg => Tree)(using Context): CpsTree = {
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
              tctx,
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
    val retval = genPrefixes(argss, CpsTree.pure(tctx,origin,owner,pureReply))
    println(s"genApplication result: ${retval.show}")
    retval

  }

  


  def makeArgList(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, tctx: TransformationContext)(using Context): ApplyTermArgList = {
    // need to calculate dependency between arguments.
    ApplyTermArgList.make(term, mt, owner, tctx)
  }


  def makeTypeArgList(term: TypeApply)(using Context): ApplyTypeArgList = {
    ApplyTypeArgList(term,term.args.map(tt => TypeTree(tt.tpe)))
  }


  def retrieveShiftedFun(fun:Tree, tctx: TransformationContext, owner:Symbol)(using Context): Tree = {

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
      ???
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