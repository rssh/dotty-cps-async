package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*

import cps.plugin.*


object MatchTransform {

  def apply(term: Match, owner:Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    //Log.trace(s"MatchTransform, term=${term.show}",nesting)
    term match
      case Match(selector, cases) =>
        val selectorCps = RootTransform(selector, owner, nesting+1)
        val casesCps = cases.map( c => CpsCaseDef(c,RootTransform(c.body,owner,nesting+1)) )
        val casesAsyncKind = collectCasesAsyncKind(casesCps)
        val casesPrepared = casesCps.map(_.prepareCaseDef(casesAsyncKind))
        val retval = selectorCps.asyncKind match
          case AsyncKind.Sync =>
            if (casesAsyncKind == AsyncKind.Sync  && casesCps.forall(_.cpsBody.isOriginEqSync)) then
              CpsTree.unchangedPure(term,owner)
            else
             val selectorPrepared = selectorCps.unpure.get
             val res = Match(selectorPrepared, casesPrepared).withSpan(term.span)
             casesAsyncKind match
               case AsyncKind.Sync =>
                 CpsTree.pure(term,owner,res)
               case AsyncKind.Async(internalKind) =>
                 CpsTree.impure(term,owner,res,internalKind)
               case AsyncKind.AsyncLambda(bodyKind) =>
                 CpsTree.opaqueAsyncLambda(term,owner,res,bodyKind)
          case AsyncKind.Async(internalKind) =>
            val nSym = Symbols.newSymbol(owner, "xMathSelect".toTermName, Flags.EmptyFlags, selectorCps.originType.widen, Symbols.NoSymbol)
            val nValDef = ValDef(nSym).withSpan(selector.span)
            casesAsyncKind match
              case AsyncKind.Sync =>
                MapCpsTree(term,owner,selectorCps,
                  MapCpsTreeArgument(Some(nValDef), CpsTree.pure(term,owner,Match(ref(nSym),casesPrepared))))
              case AsyncKind.Async(internalKind2) =>
                FlatMapCpsTree(term,owner,selectorCps,
                  FlatMapCpsTreeArgument(Some(nValDef), CpsTree.impure(term,owner,Match(ref(nSym),casesPrepared), internalKind2)))
              case AsyncKind.AsyncLambda(bodyKind) =>
                MapCpsTree(term, owner, selectorCps,
                  MapCpsTreeArgument(Some(nValDef), CpsTree.impure(term, owner, Match(ref(nSym), casesPrepared), casesAsyncKind)))
          case AsyncKind.AsyncLambda(internalKind) =>
            throw CpsTransformException("AsyncLambda as selector of match statement is not supported", term.srcPos)

        retval
      case null =>
        throw CpsTransformException("Match term expected", term.srcPos)
  }

  case class CpsCaseDef(origin: CaseDef, cpsBody: CpsTree) {

    def prepareCaseDef(targetKind: AsyncKind)(using Context, CpsTopLevelContext): CaseDef =
      CaseDef(origin.pat, origin.guard, prepareBody(targetKind))

    def prepareBody(targetKind: AsyncKind)(using Context, CpsTopLevelContext): Tree = {
      targetKind match
        case AsyncKind.Sync =>
          cpsBody.unpure.get
        case AsyncKind.Async(internalKind) =>
          cpsBody.asyncKind match
            case AsyncKind.Sync =>
                  cpsBody.transformed
            case AsyncKind.Async(internalKind2) =>
                  if (internalKind == AsyncKind.Sync) then
                    cpsBody.transformed
                  else
                    throw CpsTransformException(s"complex shape is not supported.", origin.srcPos)
            case AsyncKind.AsyncLambda(_) =>
                  throw CpsTransformException("can't convert AsyncLambda to plain async case", origin.srcPos)
        case AsyncKind.AsyncLambda(internalKind1) =>
          cpsBody.asyncKind match
            case AsyncKind.Sync =>
                  // TODO: represent as lambsa
                  throw CpsTransformException("can't convert sync case to asyncLambda", origin.srcPos)
            case AsyncKind.Async(internalKind2) =>
                  throw CpsTransformException(s"can't convert asysync case to asyncLambda", origin.srcPos)
            case AsyncKind.AsyncLambda(internalKind2) =>
                   if (internalKind1 == internalKind2) then
                      cpsBody.transformed
                   else
                      throw CpsTransformException(s"can't convert asyncLambda case to asyncLambda with different internal kind", origin.srcPos)
    }


  }


  def collectCasesAsyncKind(value: List[CpsCaseDef]): AsyncKind =
    value.foldLeft(AsyncKind.Sync) { (acc, c) =>
       acc.unify(c.cpsBody.asyncKind) match
         case Right(x) => x
         case Left(msg) => throw CpsTransformException("Can't unify async shape in case branches for match", c.origin.srcPos)
    }

}
