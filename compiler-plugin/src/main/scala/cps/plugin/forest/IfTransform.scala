package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Symbols.*
import core.Decorators.*
import core.Definitions.*
import core.StdNames
import ast.tpd.*

import cps.plugin.*

object IfTransform {


      def apply(ifTerm: If, owner: Symbol, ctx: TransformationContext)(using Context): CpsTree = {
        val cpsCond = RootTransform(ifTerm.cond, owner, ctx)
        val cpsIfTrue = RootTransform(ifTerm.thenp, owner, ctx)
        val cpsIfFalse = RootTransform(ifTerm.elsep, owner, ctx)
        cpsCond.unpure match
          case Some(condSync) =>
            (cpsIfTrue.unpure,  cpsIfFalse.unpure) match
              case (Some(ifTrueSync), Some(ifFalseSync)) =>
                if ( !cpsCond.isOriginEqSync || !cpsIfTrue.isOriginEqSync || !cpsIfFalse.isOriginEqSync) then
                  PureCpsTree(
                    ctx,
                    ifTerm,
                    owner,
                    cpy.If(ifTerm)(condSync,ifTrueSync,ifFalseSync),
                  )
                else
                  PureCpsTree(
                    ctx,
                    ifTerm,
                    owner,
                    ifTerm
                  )
              case _ =>
                  if (cpsIfTrue.asyncKind != cpsIfFalse.asyncKind) then
                    throw CpsTransformException("Different async kind in if branch",ifTerm.srcPos)
                  AsyncTermCpsTree(
                    ctx,
                    ifTerm,
                    owner,
                    cpy.If(ifTerm)(condSync, cpsIfTrue.transformed, cpsIfFalse.transformed),
                    cpsIfTrue.asyncKind
                  )
          case None =>
            val sym = newSymbol(owner, "c".toTermName , Flags.EmptyFlags, defn.BooleanType)
            val valDef = ValDef(sym.asTerm, EmptyTree)
            (cpsIfTrue.unpure,  cpsIfFalse.unpure) match
              case (Some(ifTrueSync), Some(ifFalseSync)) =>
                val newIf = If(ref(sym),ifTrueSync,ifFalseSync).withSpan(ifTerm.span)
                MapCpsTree(
                  ctx,
                  ifTerm,
                  owner,
                  cpsCond,
                  MapCpsTreeArgument(Some(valDef), CpsTree.pure(ctx,ifTerm,owner,newIf))
                )
              case _ =>
                if (cpsIfTrue.asyncKind != cpsIfFalse.asyncKind) then
                   throw CpsTransformException("Different async kind in if branches",ifTerm.srcPos) 
                val newIf = If(ref(sym),cpsIfTrue.transformed,cpsIfFalse.transformed)
                              .withSpan(ifTerm.span)        
                val cpsNewIf = AsyncTermCpsTree(ctx,ifTerm, owner, newIf, cpsIfTrue.asyncKind)              
                FlatMapCpsTree(
                  ctx,
                  ifTerm,
                  owner,
                  cpsCond,
                  FlatMapCpsTreeArgument(Some(valDef), cpsNewIf)
                )
     }

}