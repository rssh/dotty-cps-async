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


      def apply(ifTerm: If, owner:Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
        val cpsCond = RootTransform(ifTerm.cond, owner, nesting+1)
        val cpsIfTrue = RootTransform(ifTerm.thenp, owner, nesting+1)
        val cpsIfFalse = RootTransform(ifTerm.elsep, owner,  nesting+1)
        cpsCond.unpure match
          case Some(condSync) =>
            (cpsIfTrue.unpure,  cpsIfFalse.unpure) match
              case (Some(ifTrueSync), Some(ifFalseSync)) =>
                if ( !cpsCond.isOriginEqSync || !cpsIfTrue.isOriginEqSync || !cpsIfFalse.isOriginEqSync) then
                  PureCpsTree(
                    ifTerm,
                    owner,
                    cpy.If(ifTerm)(condSync,ifTrueSync,ifFalseSync)
                  )
                else
                  PureCpsTree(
                    ifTerm,
                    owner,
                    ifTerm
                  )
              case _ =>
                  if ( !cpsIfTrue.asyncKind.isCompatible(cpsIfFalse.asyncKind) ) then
                    report.error(
                      s"""
                        |Different async kind in if branches
                        |If: ${ifTerm.show}
                        |leftKind: ${cpsIfTrue.asyncKind}
                        |rightKind: ${cpsIfFalse.asyncKind}
                        |""".stripMargin
                    )
                    throw CpsTransformException("Different async kind in if branch",ifTerm.srcPos)
                  AsyncTermCpsTree(
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
                  ifTerm,
                  owner,
                  cpsCond,
                  MapCpsTreeArgument(Some(valDef), CpsTree.pure(ifTerm,owner,newIf))
                )
              case _ =>
                if (!cpsIfTrue.asyncKind.isCompatible(cpsIfFalse.asyncKind)) then
                   throw CpsTransformException(s"Different async kind in if branches ${cpsIfTrue.asyncKind} and ${cpsIfFalse.asyncKind}",ifTerm.srcPos)
                val newIf = If(ref(sym),cpsIfTrue.transformed,cpsIfFalse.transformed)
                              .withSpan(ifTerm.span)        
                val cpsNewIf = AsyncTermCpsTree(ifTerm, owner, newIf, cpsIfTrue.asyncKind)
                FlatMapCpsTree(
                  ifTerm,
                  owner,
                  cpsCond,
                  FlatMapCpsTreeArgument(Some(valDef), cpsNewIf)
                )
     }

}