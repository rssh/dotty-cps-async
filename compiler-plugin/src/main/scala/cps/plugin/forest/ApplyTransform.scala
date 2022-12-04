package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

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

  def parseApplication(appTerm: Apply, owner: Symbol, tctx: TransformationContext, args: List[ApplyArgList])(using Context): CpsTree = {
      val cpsApplicant = RootTransform(appTerm.fun ,owner, tctx)
      cpsApplicant.unpure match
        case Some(syncFun) => parseSyncApplication(appTerm, owner, tctx, syncFun, args)
        case None =>
          val haveAsync = args.exists(_.isAsync)
          val valDefSym = newSymbol(owner, "x".toTermName, Flags.EmptyFlags, 
                          cpsApplicant.originType.widen, Symbols.NoSymbol)
          val valDef = ValDef(valDefSym, EmptyTree).withSpan(appTerm.span)
          val valRef = ref(valDefSym)
          if (haveAsync) {
            FlatMapCpsTree(tctx,appTerm,owner,cpsApplicant,FlatMapCpsTreeArgument(Some(valDef),parseSyncApplication(appTerm,owner,tctx,valRef,args)))
          } else {
            MapCpsTree(tctx,appTerm,owner,cpsApplicant,MapCpsTreeArgument(Some(valDef), parseSyncPureApplication(appTerm,owner,tctx,valRef,args)))
          }
  }

  def parseSyncApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, args:List[ApplyArgList])(using Context): CpsTree = {
    ???
  }

  def parseSyncPureApplication(origin: Apply, owner: Symbol, tctx: TransformationContext, fun: Tree, args:List[ApplyArgList])(using Context): CpsTree = {
    ???
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


}