package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span

import cps.plugin.*

import cps.plugin.forest.application.*

object ApplyTransform {

  def apply(term: Apply, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
        applyMArgs(term,owner,tctx, Nil)
  }


  def applyMArgs(term: Apply, owner: Symbol, tctx: TransformationContext, tail:List[ApplyArgList] )(using Context): CpsTree = {
    val argList = makeArgList(term, MethodParamsDescriptor(term.fun), owner, tctx)
    term.fun match
      case tfa@Apply(fun1,args1) => 
        applyMArgs(tfa, owner, tctx, argList::tail)
      case tpfa@TypeApply(tapp:Apply, targs1) =>
        val targs = makeTypeArgList(tpfa)
        applyMArgs(tapp, owner, tctx, targs::argList::tail)  
      case _ => parseApplication(term,owner,tctx,tail)
  }

  def parseApplication(appTerm: Apply, owner: Symbol, tctx: TransformationContext, argss: List[ApplyArgList])(using Context): CpsTree = {
      val cpsApplicant = RootTransform(appTerm.fun ,owner, tctx)
      cpsApplicant.unpure match
        case Some(syncFun) => parseSyncApplication(appTerm, owner, tctx, syncFun, argss)
        case None =>
          val haveAsyncOrShifted = argss.exists(a => a.isAsync||a.containsAsyncLambda)
          val valDefSym = newSymbol(owner, "x".toTermName, Flags.EmptyFlags, 
                          cpsApplicant.originType.widen, Symbols.NoSymbol)
          val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
          val valRef = ref(valDefSym)
          if (haveAsyncOrShifted) {
            FlatMapCpsTree(tctx,appTerm,owner,cpsApplicant,FlatMapCpsTreeArgument(Some(valDef),parseSyncApplication(appTerm,owner,tctx,valRef,argss)))
          } else {
            MapCpsTree(tctx,appTerm,owner,cpsApplicant,MapCpsTreeArgument(Some(valDef), parseSyncPureApplication(appTerm,owner,tctx,valRef,argss)))
          }
  }

  def parseSyncApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, args:List[ApplyArgList])(using Context): CpsTree = {
      val containsAsyncLambda = args.exists(_.containsAsyncLambda)
      if (containsAsyncLambda) {
        findRuntimeAwait(tctx, origin.span) match
          case Some(runtimeAwait) =>
            genApplication(origin,owner,tctx,fun,args, arg => arg.exprInCall(false,Some(runtimeAwait)))
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
                  genApplication(origin, owner, tctx, changedFun, args, arg => arg.identWithShift)
            } else {
              fun match
                case QuoteLikeApi.CheckLambda(params,body,bodyOwner) =>
                  // Lambda transdorm
                  ???
                case _ =>  
                  throw CpsTransformException(s"Can't transform function ${fun}",fun.srcPos)
            }
      } else {
        genApplication(origin, owner, tctx, fun, args, arg > arg.ident)
      }
  }

  //  just unchanged
  def parseSyncPureApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, args:List[ApplyArgList])(using Context): CpsTree = {
     args.fold(fun){ (s,e) =>
        e match
          case ApplyTypeArgList(orig,args) =>
            TypeApply(s,args).withSpan(orig.span)
          case ApplyTermArgList(orig,args) =>
            Apply(s,args).withSpan(orig.span)
     }
  }

  def genApplication(origin:Tree, owner: Symbol, tctx: TransformationContext, fun: Tree, args: List[ApplyArgList], f: ApplyArg => Tree): CpsTree = {
    if (args.exists(_.isAsync)) {
       ???
    }
    args.fold(fun){ (s,eArgs) =>
      eArgs.args.fold(s){ (s,e) =>
        if (e.isAsync) {
          
        }

      }

    }
  }

  


  def makeArgList(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, tctx: TransformationContext)(using Context): ApplyTermArgList = {
    ApplyTermArgList(term, term.args.zipWithIndex.map((a,i) => ApplyArg(a, i, 
                mt.paramName(i, a.srcPos).toTermName,  
                mt.paramType(i, a.srcPos),
                mt.isByName(i, a.srcPos),
                owner,
                tctx
    ) ))
  }

  def makeTypeArgList(term: TypeApply): ApplyTypeArgList = {
    ApplyTypeArgList(term,term.args.map(_.tpe))
  }

  def findRuntimeAwait(tctx: TransformationContext, span: Span)(using ctx:Context): Option[Tree] = {
    val runtimeAwait = TypeRef(RequiredClass("cps.RuntimeAwait"))
    val tpe = AppliedType(runtimeAwait, List(tctx.monadType))
    val searchResult = ctx.types.inferImplicits(tpe,span)
    searchResult.tpe match
      case _ : dotc.typer.Implicits.SearchFailureType => None
      case _  => Some(searchResult)
  }


}