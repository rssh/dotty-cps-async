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


      def apply(ifTerm: If, oldOwner: Symbol, newOwner:Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
        val cpsCond = RootTransform(ifTerm.cond, oldOwner, newOwner, nesting+1)
        val cpsIfTrue = RootTransform(ifTerm.thenp, oldOwner, newOwner, nesting+1)
        val cpsIfFalse = RootTransform(ifTerm.elsep, oldOwner, newOwner, nesting+1)
        cpsCond.unpure match
          case Some(condSync) =>
            (cpsIfTrue.unpure,  cpsIfFalse.unpure) match
              case (Some(ifTrueSync), Some(ifFalseSync)) =>
                if ( !cpsCond.isOriginEqSync || !cpsIfTrue.isOriginEqSync || !cpsIfFalse.isOriginEqSync) then
                  PureCpsTree(
                    ifTerm,
                    newOwner,
                    cpy.If(ifTerm)(condSync,ifTrueSync,ifFalseSync).changeOwner(oldOwner,newOwner)
                  )
                else
                  val ifTermNewOwner = ifTerm.changeOwner(oldOwner,newOwner)
                  PureCpsTree(
                    ifTermNewOwner,
                    newOwner,
                    ifTermNewOwner
                  )
              case _ =>
                  if (cpsIfTrue.asyncKind != cpsIfFalse.asyncKind) then
                    throw CpsTransformException("Different async kind in if branch",ifTerm.srcPos)
                  AsyncTermCpsTree(
                    ifTerm,
                    newOwner,
                    cpy.If(ifTerm)(condSync, cpsIfTrue.transformed, cpsIfFalse.transformed),
                    cpsIfTrue.asyncKind
                  )
          case None =>
            val sym = newSymbol(newOwner, "c".toTermName , Flags.EmptyFlags, defn.BooleanType)
            val valDef = ValDef(sym.asTerm, EmptyTree)
            (cpsIfTrue.unpure,  cpsIfFalse.unpure) match
              case (Some(ifTrueSync), Some(ifFalseSync)) =>
                val newIf = If(ref(sym),ifTrueSync,ifFalseSync).withSpan(ifTerm.span)
                MapCpsTree(
                  ifTerm,
                  newOwner,
                  cpsCond,
                  MapCpsTreeArgument(Some(valDef), CpsTree.pure(ifTerm,newOwner,newIf.changeOwner(oldOwner,newOwner)))
                )
              case _ =>
                if (cpsIfTrue.asyncKind != cpsIfFalse.asyncKind) then
                   throw CpsTransformException("Different async kind in if branches",ifTerm.srcPos) 
                val newIf = If(ref(sym),cpsIfTrue.transformed,cpsIfFalse.transformed)
                              .withSpan(ifTerm.span)        
                val cpsNewIf = AsyncTermCpsTree(ifTerm, newOwner, newIf, cpsIfTrue.asyncKind)
                FlatMapCpsTree(
                  ifTerm,
                  newOwner,
                  cpsCond,
                  FlatMapCpsTreeArgument(Some(valDef), cpsNewIf)
                )
     }

}