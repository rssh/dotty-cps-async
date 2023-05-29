package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*

import cps.plugin.*
import cps.plugin.forest.cases.*

object MatchTransform {

  def apply(term: Match, owner:Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"MatchTransform, term=${term.show}",nesting)
    term match
      case Match(selector, cases) =>
        val selectorCps = RootTransform(selector, owner, nesting+1)
        val casesCps =  CpsCases.create(cases, owner, nesting+1)
        val casesAsyncKind = casesCps.collectAsyncKind
        val casesPrepared = casesCps.transformedCaseDefs(casesAsyncKind, term.tpe)
        val retval = selectorCps.asyncKind match
          case AsyncKind.Sync =>
            if (casesAsyncKind == AsyncKind.Sync  && casesCps.unchanged) then
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



}
