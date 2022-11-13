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
        if (!cpsCond.isAsync) then
          if (!cpsIfTrue.isAsync &&  !cpsIfTrue.isAsync) then
            if (cpsCond.isOriginChanged || cpsIfTrue.isOriginChanged || cpsIfFalse.isOriginChanged) then
              PureCpsTree(
                ctx.monadType,
                ctx.cpsMonadRef,
                cpy.If(ifTerm)(cpsCond.changedOrigin,cpsIfTrue.changedOrigin,cpsIfFalse.changedOrigin)
              )
            else
              PureCpsTree(
                ctx.monadType,
                ctx.cpsMonadRef,
                ifTerm
              )
          else 
            AsyncTermCpsTree(
              ctx.monadType,
              ctx.cpsMonadRef,
              ifTerm,
              cpy.If(ifTerm)(cpsCond.changedOrigin, cpsIfTrue.transformed, cpsIfFalse.transformed)
            )
        else 
          if (!cpsIfTrue.isAsync &&  !cpsIfTrue.isAsync) then
            val mt = MethodType(List("c".toTermName))(_ => List(defn.BooleanType), _ => ifTerm.tpe)
            val lambda = Lambda(mt, 
                 params=>cpy.If(ifTerm)(params.head,cpsIfTrue.changedOrigin,cpsIfFalse.changedOrigin) 
              )
            MapCpsTree(
              ctx.monadType,
              ctx.cpsMonadRef,
              ifTerm,
              cpsCond,
              lambda
            )
          else
            val mt = MethodType(List("c".toTermName))(_ => List(defn.BooleanType), _ => AppliedType(ctx.monadType, List(ifTerm.tpe)) )
            val lambda = Lambda(mt, 
                 params=>cpy.If(ifTerm)(params.head,cpsIfTrue.transformed,cpsIfFalse.transformed) 
              )
            FlatMapCpsTree(
              ctx.monadType,
              ctx.cpsMonadRef,
              ifTerm,
              cpsCond,
              lambda
            )  
      }

}