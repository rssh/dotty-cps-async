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
                  AsyncTermCpsTree(
                    ctx,
                    ifTerm,
                    owner,
                    cpy.If(ifTerm)(condSync, cpsIfTrue.transformed, cpsIfFalse.transformed)
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
                  MapCpsTreeArgument(valDef, CpsTree.pure(ctx,ifTerm,owner,newIf))
                )
              case _ =>
                val newIf = If(ref(sym),cpsIfTrue.transformed,cpsIfFalse.transformed)
                              .withSpan(ifTerm.span)
                FlatMapCpsTree(
                  ctx,
                  ifTerm,
                  owner,
                  cpsCond,
                  FlatMapCpsTreeArgument(valDef, AsyncTermCpsTree(ctx,ifTerm,owner,newIf) )
                )
     }

}