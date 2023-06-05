package cps.plugin.forest.cases

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*

import cps.plugin.*
import cps.plugin.forest.*


case class CpsCaseDef(origin: CaseDef, cpsBody: CpsTree) {

  def transform(targetKind: AsyncKind, targetType: Type)(using Context, CpsTopLevelContext): CaseDef =
    CaseDef(origin.pat, origin.guard, transformBody(targetKind, targetType))

  def transformBody(targetKind: AsyncKind, originTargetType: Type)(using Context, CpsTopLevelContext): Tree = {
    targetKind match
      case AsyncKind.Sync =>
        cpsBody.castOriginType(originTargetType).unpure.get
      case AsyncKind.Async(internalKind) =>
        cpsBody.asyncKind match
          case AsyncKind.Sync =>
            cpsBody.castOriginType(originTargetType).transformed
          case AsyncKind.Async(internalKind2) =>
            if (internalKind == AsyncKind.Sync) then
              cpsBody.castOriginType(originTargetType).transformed
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
              cpsBody.castOriginType(originTargetType).transformed
            else
              throw CpsTransformException(s"can't convert asyncLambda case to asyncLambda with different internal kind", origin.srcPos)
  }


}

