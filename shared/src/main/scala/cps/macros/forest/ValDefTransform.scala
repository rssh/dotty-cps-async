package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


object ValDefTransform:


  def fromBlock[F[_]:Type,C<:CpsMonadContext[F]:Type](using Quotes)(
                           cpsCtx: TransformationContext[F,Unit,C],
                           valDef: quotes.reflect.ValDef): CpsExpr[F,Unit] = {
     import quotes.reflect._
     import cpsCtx._
     if cpsCtx.flags.debugLevel >= 15 then
       cpsCtx.log(s"ValDefExpr:fromBlock, valDef=$valDef")
     val rhs = valDef.rhs.getOrElse(
             throw MacroError(s"val $valDef without right part in block ", cpsCtx.patternCode)
     )
     val rhsType = TransformUtil.veryWiden(rhs.tpe).asType
     rhsType match 
        case '[et] =>
            if cpsCtx.flags.debugLevel > 15 then
               cpsCtx.log(s"ValDef::rightPart is ${TransformUtil.safeShow(rhs)}")
            val cpsRight = Async.nestTransform(rhs.asExprOf[et],cpsCtx)
            val memCpsRight:CpsExpr[F,et] = if (cpsCtx.flags.automaticColoring 
                                              && cpsCtx.memoization.isDefined
                                              && cpsCtx.memoization.get.kind != CpsMonadMemoization.Kind.BY_DEFAULT) {
               rhsType match
                 case '[F[r]] =>
                    val refinedCpsRight = cpsRight.asInstanceOf[CpsExpr[F,F[r]]]
                    val analysis = cpsCtx.observatory.effectColoring
                    val usageRecord = analysis.usageRecords.get(valDef.symbol).getOrElse{
                              val msg = "Can't find analysis record for usage of ${valDef.symbol}"
                              throw MacroError(msg, rhs.asExpr)
                    } 
                    if (usageRecord.nInAwaits > 0 && usageRecord.nWithoutAwaits > 0) then
                        // TODO:  diagnostic
                        report.error(s"value ${valDef.symbol} passed in sync and async form at the same time",valDef.pos)
                        usageRecord.reportCases()
                    val toMemoize = usageRecord.nInAwaits > 0
                    if (toMemoize) then
                      val memoization = cpsCtx.memoization.get
                      memoization.kind match
                        case CpsMonadMemoization.Kind.BY_DEFAULT => cpsRight
                        case CpsMonadMemoization.Kind.INPLACE => 
                           val mm = memoization.monadMemoization.asExprOf[CpsMonadMemoization.Inplace[F]]
                           if (cpsRight.isAsync) then
                              refinedCpsRight.map( '{ (x:F[r]) => ${mm}.apply(x) } ).asInstanceOf[CpsExpr[F,et]]
                           else
                              val rhsExpr = cpsRight.syncOrigin.get
                              val nextRhs = '{ ${mm}.apply( ${rhsExpr.asExprOf[F[r]]} ) }
                              CpsExpr.sync(monad, nextRhs, changed=true).asInstanceOf[CpsExpr[F,et]]
                        case CpsMonadMemoization.Kind.PURE => 
                           val mm = memoization.monadMemoization.asExprOf[CpsMonadMemoization.Pure[F]]
                           if (cpsRight.isAsync) then
                              refinedCpsRight.flatMap( '{ (x:F[r]) => ${mm}.apply(x) } ).asInstanceOf[CpsExpr[F,et]]
                           else
                              val rhsExpr = cpsRight.syncOrigin.get
                              val nextRhs = '{ ${mm}.apply( ${rhsExpr.asExprOf[F[r]]} ) }
                              CpsExpr.async(monad, nextRhs.asExprOf[F[F[r]]]).asInstanceOf[CpsExpr[F,et]]
                        case CpsMonadMemoization.Kind.DYNAMIC => 
                           Expr.summon[CpsMonadMemoization.DynamicAp[F,r,et]] match
                               case Some(mm) => 
                                 if (cpsRight.isAsync) then
                                    refinedCpsRight.flatMap( '{ (x:F[r]) => ${mm}.apply(x.asInstanceOf[et]) } ).asInstanceOf[CpsExpr[F,et]]
                                 else
                                    val rhsExpr = cpsRight.syncOrigin.get
                                    val nextRhs = '{ ${mm}.apply( ${rhsExpr} ) }
                                    CpsExpr.async(monad, nextRhs.asExprOf[F[et]]).asInstanceOf[CpsExpr[F,et]]
                               case None =>
                                 // todo: use search instead summon for additional message in failure
                                 val msg = 
                                   s"Can't find given instance of ${TypeRepr.of[CpsMonadMemoization.DynamicAp[F,r,et]].show}"
                                 throw MacroError(msg, rhs.asExpr)
                    else
                      cpsRight
                 case _ => cpsRight
            } else {
               cpsRight
            }
            if (memCpsRight.isAsync) then
               if (cpsCtx.flags.debugLevel > 15) 
                  cpsCtx.log(s"rightPart is async")
               RhsFlatMappedCpsExpr(using quotes)(monad, Seq(), valDef, memCpsRight, CpsExpr.unit(monad))
            else 
               if (cpsCtx.flags.debugLevel > 15) 
                 cpsCtx.log(s"ValDef: rightPart no async, rhs.isChanged = ${memCpsRight.isChanged}  memCpsRight.transformed=${TransformUtil.safeShow(memCpsRight.transformed.asTerm)}")
               val rhsTerm = memCpsRight.syncOrigin.get.asTerm
               val nextValDef = if (memCpsRight.isChanged) {
                                     ValDef(valDef.symbol, Some(rhsTerm.changeOwner(valDef.symbol)))
                                } else 
                                     valDef
               ValWrappedCpsExpr(using quotes)(monad, Seq(), nextValDef,   CpsExpr.unit(monad) )
        case other =>
            throw MacroError(s"Can't concretize type of right-part $rhs ", rhs.asExpr)


  }


  class RhsFlatMappedCpsExpr[F[_]:Type, T:Type, V:Type](using thisQuotes: Quotes)
                                     (monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: quotes.reflect.ValDef,
                                      cpsRhs: CpsExpr[F,V],
                                      next: CpsExpr[F,T]
                                     )
                                    extends AsyncCpsExpr[F,T](monad, prev) {

       override def fLast(using Quotes) =
          import quotes.reflect._
         
          next.syncOrigin match
            case Some(nextOrigin) =>
             '{
               ${monad}.map(${cpsRhs.transformed})((v:V) =>
                          ${buildAppendBlockExpr('v, nextOrigin)})
              }
            case  None =>
             '{
               ${monad}.flatMap(${cpsRhs.transformed})((v:V)=>
                          ${buildAppendBlockExpr('v, next.transformed)})
             }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty)
             this
          else
             RhsFlatMappedCpsExpr(using thisQuotes)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:quoted.Type](e: CpsExpr[F,A])(using Quotes) =
          RhsFlatMappedCpsExpr(using thisQuotes)(monad,prev,oldValDef,cpsRhs,next.append(e))


       private def buildAppendBlock(using Quotes)(rhs:quotes.reflect.Term,
                                                  exprTerm:quotes.reflect.Term): quotes.reflect.Term =
       {
          import quotes.reflect._
          import scala.quoted.Expr

          val castedOldValDef = oldValDef.asInstanceOf[quotes.reflect.ValDef]
          val valDef = ValDef(castedOldValDef.symbol, Some(rhs.changeOwner(castedOldValDef.symbol)))
          exprTerm.changeOwner(castedOldValDef.symbol.owner) match 
              case Block(stats,last) =>
                    last match
                       case Closure(mech,optType) => 
                          Block(valDef::Nil,Block(stats,last))
                       case _ =>   
                          Block(valDef::stats, last)
              case other =>
                    Block(valDef::Nil,other)

       }

       private def buildAppendBlockExpr[A:Type](using Quotes)(rhs: Expr[V], expr:Expr[A]):Expr[A] =
          import quotes.reflect._
          buildAppendBlock(rhs.asTerm,expr.asTerm).asExprOf[A]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](using Quotes)(
                                      monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: quotes.reflect.ValDef,
                                      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev):


       override def isAsync = next.isAsync

       override def syncOrigin(using Quotes): Option[Expr[T]] = next.syncOrigin.map{ n =>
         import quotes.reflect._
         val valDef: Statement = oldValDef.asInstanceOf[quotes.reflect.ValDef]
         val prevStats: List[Statement] = prev.map(_.extract.changeOwner(valDef.symbol.owner)).toList
         val outputTerm = n.asTerm.changeOwner(valDef.symbol.owner) match
            case block@Block(statements, last) =>
                   TransformUtil.prependStatementsToBlock(prevStats ++: List(valDef), block )
            case other =>
                   Block( prevStats ++: List(valDef), other)
         outputTerm.asExprOf[T]
       }


       override def fLast(using Quotes) = next.fLast

       override def transformed(using Quotes) = {
          import quotes.reflect._

          val valDef = oldValDef.asInstanceOf[quotes.reflect.ValDef]
          val prevWithNewOwner = prev.map(_.extract.changeOwner(valDef.symbol.owner))
          val block = next.transformed.asTerm.changeOwner(valDef.symbol.owner) match 
             case block@Block(stats, e) =>
                  TransformUtil.prependStatementsToBlock(prevWithNewOwner ++: List(valDef), block)
             case other =>
                 Block( prevWithNewOwner ++: List(valDef) , other)
          block.asExprOf[F[T]]

       }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty)
            this
          else
            ValWrappedCpsExpr[F,T,V](using quotes)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(using Quotes) =
           ValWrappedCpsExpr(using quotes)(monad, prev,
                                         oldValDef.asInstanceOf[quotes.reflect.ValDef],
                                         next.append(e))


       def prependPrev(using qctx:Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
          import quotes.reflect._
          if (prev.isEmpty) {
             term
          } else {
             val prevExtracted = prev.toList.map(_.extract)
             val retval = TransformUtil.prependStatementsToTerm(prevExtracted, term) 
             retval.changeOwner(Symbol.spliceOwner)
          }


