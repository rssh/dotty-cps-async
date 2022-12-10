package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


trait ValDefTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import quotes.reflect.*

  def runValDefFromBlock(block: Block, valDef: ValDef)(owner: Symbol): CpsTree = {
    if cpsCtx.flags.debugLevel >= 15 then
      cpsCtx.log(s"ValDefTreeTransform:runValDefFomBlock, valDef=$valDef")
    val rhs = valDef.rhs.getOrElse(
            throw MacroError(s"val $valDef without right part in block ", block.asExpr)
    )
    //val rhsType = TransformUtil.veryWiden(rhs.tpe).asType
    val cpsRhs = runRoot(rhs)(valDef.symbol)
    val memRhs = valDefApplyMemoization(valDef, cpsRhs, rhs)
    ValCpsTree(owner,valDef, memRhs, CpsTree.empty, true)
  }

  
  def valDefApplyMemoization(valDef: ValDef, cpsRhs: CpsTree, rhs:Term): CpsTree = {
    if (cpsCtx.flags.automaticColoring 
      && cpsCtx.memoization.isDefined
      && cpsCtx.memoization.get.kind != CpsMonadMemoization.Kind.BY_DEFAULT) {
      val rhsTpe = TransformUtil.veryWiden(rhs.tpe)
      rhsTpe.asType match 
        case '[F[r]] =>
          val analysis = cpsCtx.observatory.effectColoring
          val usageRecord = analysis.usageRecords.get(valDef.symbol).getOrElse{
                              val msg = "Can't find analysis record for usage of ${valDef.symbol}"
                              throw MacroError(msg, rhs.asExpr)
          } 
          if (usageRecord.nInAwaits > 0 && usageRecord.nWithoutAwaits > 0) then {
            // TODO:  diagnostic
            report.error(s"value ${valDef.symbol} passed in sync and async form at the same time",valDef.pos)
            usageRecord.reportCases()
          }
          val toMemoize = usageRecord.nInAwaits > 0
          if (toMemoize) then 
            given Quotes = valDef.symbol.asQuotes
            val memoization = cpsCtx.memoization.get
            memoization.kind match
              case CpsMonadMemoization.Kind.BY_DEFAULT => cpsRhs
              case CpsMonadMemoization.Kind.INPLACE => 
                val mm = memoization.monadMemoization.asExprOf[CpsMonadMemoization.Inplace[F]]
                cpsRhs.syncOrigin match
                  case Some(t) =>
                    PureCpsTree( valDef.symbol, '{ ${mm}.apply(${t.asExprOf[F[r]]}) }.asTerm )
                  case None =>  
                    cpsRhs.monadMap( t => '{ ${mm}.apply(${t.asExprOf[F[r]]}) }.asTerm , rhsTpe )
              case CpsMonadMemoization.Kind.PURE =>
                val mm = memoization.monadMemoization.asExprOf[CpsMonadMemoization.Pure[F]]
                // TODO: recheck
                cpsRhs.monadFlatMap( t => '{ ${mm}.apply(${t.asExprOf[F[r]]}) }.asTerm, rhsTpe)
              case CpsMonadMemoization.Kind.DYNAMIC =>
                rhsTpe.asType match
                  case '[et] =>
                    Expr.summon[CpsMonadMemoization.DynamicAp[F,r,et]] match
                      case Some(mm) =>
                        //TODO: recheck
                        cpsRhs.monadFlatMap( t => '{ ${mm}.apply(${t.asExprOf[et]}) }.asTerm, rhsTpe )
                      case None =>
                        // todo: use search instead summon for additional message in failure
                        val msg = s"Can't find given instance of ${TypeRepr.of[CpsMonadMemoization.DynamicAp[F,r,et]].show}"
                        throw MacroError(msg, rhs.asExpr)
                  case _ =>
                    throw MacroError(s"Can't determinate type for ${rhs.show}", rhs.asExpr)
          else 
            cpsRhs
        case _ => 
          cpsRhs
    } else {
      cpsRhs
    }

  }
