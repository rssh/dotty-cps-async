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


      def apply(term: ValDef, owner: Symbol, tctx: TransformationContext, nesting:Int)(using Context): CpsTree = {

            if (term.rhs.isEmpty) then
                  throw CpsTransformException("ValDef without right part", term.srcPos)
            val cpsRhs = RootTransform(term.rhs,term.symbol,tctx,nesting+1)
            cpsRhs.asyncKind match
                  case AsyncKind.Sync =>
                        if (cpsRhs.isOriginEqSync) then
                              CpsTree.unchangedPure(tctx,term,owner)
                        else
                              val newValDef = cpy.ValDef(term)(name=term.name, tpt=term.tpt, rhs=cpsRhs.unpure.get)
                              CpsTree.pure(tctx,term,owner,newValDef)
                  case AsyncKind.Async(_) =>
                        val cpsRhs2 = cpsRhs.changeOwner(owner)
                        val nValDef = cpy.ValDef(term)(term.name,term.tpt,EmptyTree)
                        MapCpsTree(
                              tctx,term,owner,
                              cpsRhs2,
                              MapCpsTreeArgument(
                                    Some(nValDef),
                                    CpsTree.unit(tctx,owner)
                              )
                        )
                  case rhsFun: AsyncKind.AsyncLambda =>
                        // here value is a function.
                        tctx.optRuntimeAwait match
                              case Some(runtimeAwait) => 
                                    val newLambda = cpsRhs.applyRuntimeAwait(runtimeAwait).unpure.get
                                    val nValDef = cpy.ValDef(term)(term.name,term.tpt,rhs=newLambda)
                                    CpsTree.pure(tctx,term,owner,nValDef)
                              case None =>
                                    //we can't change types in plugin,
                                    // Theoretically it's possible to track usage of ValDef and fix xhanged,
                                    //  but let think about this after an initial release
                                    throw CpsTransformException(s"Functional variable trasnfrom is not supported",term.srcPos)


      }

}