package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object ValDefTransform {


      def apply(term: ValDef, oldOwner: Symbol, newOwner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
            val tctx = summon[CpsTopLevelContext]
            if (term.rhs.isEmpty) then
                  throw CpsTransformException(s"ValDef without right part: $term", term.srcPos)
            val cpsRhs = RootTransform(term.rhs,term.symbol,term.symbol,nesting+1)
            cpsRhs.asyncKind match
                  case AsyncKind.Sync =>
                        if (cpsRhs.isOriginEqSync) then
                              CpsTree.unchangedPure(term.changeOwner(oldOwner,newOwner),newOwner)
                        else
                              val newValDef = cpy.ValDef(term)(name=term.name, tpt=term.tpt, rhs=cpsRhs.unpure.get).changeOwner(oldOwner,newOwner)
                              CpsTree.pure(term,newOwner,newValDef)
                  case AsyncKind.Async(_) =>
                        val cpsRhs2 = cpsRhs.changeOwner(newOwner)
                        val nValDef = cpy.ValDef(term)(term.name,term.tpt,EmptyTree)
                        MapCpsTree(
                              term,
                              newOwner,
                              cpsRhs2,
                              MapCpsTreeArgument(
                                    Some(nValDef),
                                    CpsTree.unit(newOwner)
                              )
                        )
                  case rhsFun: AsyncKind.AsyncLambda =>
                        // here value is a function.
                        tctx.optRuntimeAwait match
                              case Some(runtimeAwait) => 
                                    val newLambda = cpsRhs.applyRuntimeAwait(runtimeAwait).unpure.get
                                    val nValDef = cpy.ValDef(term)(term.name,term.tpt,rhs=newLambda)
                                    CpsTree.pure(term.changeOwner(oldOwner,newOwner),newOwner,nValDef)
                              case None =>
                                    //we can't change types in plugin,
                                    // Theoretically it's possible to track usage of ValDef and fix xhanged,
                                    //  but let think about this after an initial release
                                    throw CpsTransformException(s"Functional variable trasnfrom is not supported",term.srcPos)


      }

}