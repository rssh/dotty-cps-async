package cps.forest

import scala.quoted._

import cps._
import cps.misc._


object ValDefTransform:


  def fromBlock[F[_]:Type](using qctx:QuoteContext)(
                           cpsCtx: TransformationContext[F,Unit],
                           valDef: qctx.reflect.ValDef): CpsExpr[F,Unit] = {
     import qctx.reflect._
     import cpsCtx._
     if (cpsCtx.flags.debugLevel >= 15) 
       cpsCtx.log(s"ValDefExpr:fromBlock, valDef=$valDef")
     val rhs = valDef.rhs.getOrElse(
             throw MacroError(s"val $valDef without right part in block ", cpsCtx.patternCode)
     )
     rhs.seal match 
        case '{ $e: et } =>
            if (cpsCtx.flags.debugLevel > 15) 
               cpsCtx.log(s"rightPart is ${e.show}")
            val cpsRight = Async.nestTransform(e,cpsCtx,TransformationContextMarker.ValDefRight)
            if (cpsRight.isAsync) {
               if (cpsCtx.flags.debugLevel > 15) {
                  cpsCtx.log(s"rightPart is async")
               }
               RhsFlatMappedCpsExpr(using qctx)(monad, Seq(),
                                                valDef, cpsRight, CpsExpr.unit(monad) )
            } else {
               if (cpsCtx.flags.debugLevel > 15) 
                 cpsCtx.log(s"rightPart is no async, cpsRight.transformed=${cpsRight.transformed.show}")
               ValWrappedCpsExpr(using qctx)(monad, Seq(), valDef, 
                                                CpsExpr.unit(monad) )
            }
        case other =>
            throw MacroError(s"Can't concretize type of right-part $rhs ", cpsCtx.patternCode)

     
  }


  class RhsFlatMappedCpsExpr[F[_]:Type, T:Type, V:Type](using qctx:QuoteContext)
                                     (monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: qctx.reflect.ValDef,
                                      cpsRhs: CpsExpr[F,V],
                                      next: CpsExpr[F,T]
                                     )
                                    extends AsyncCpsExpr[F,T](monad, prev) {

       override def fLast(using qctx: QuoteContext) = 
          import qctx.reflect._

          def appendBlockExpr[A:quoted.Type](rhs: qctx.reflect.Term, expr: Expr[A]):Expr[A] =
                buildAppendBlockExpr(oldValDef.asInstanceOf[qctx.reflect.ValDef],
                                     rhs, expr)

          next.syncOrigin match 
            case Some(nextOrigin) =>
             '{
               ${monad}.map(${cpsRhs.transformed})((v:V) => 
                          ${appendBlockExpr('v.unseal, nextOrigin)}) 
              }
            case  None =>
             '{
               ${monad}.flatMap(${cpsRhs.transformed})((v:V)=>
                          ${appendBlockExpr('v.unseal, next.transformed)}) 
             }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty) 
             this
          else
             RhsFlatMappedCpsExpr(using qctx)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:quoted.Type](e: CpsExpr[F,A])(using qtcx: QuoteContext) = 
          RhsFlatMappedCpsExpr(using qctx)(monad,prev,oldValDef,cpsRhs,next.append(e))
                                                          
             
       private def buildAppendBlock(using qctx:QuoteContext)(
                      oldValDef: qctx.reflect.ValDef, rhs:qctx.reflect.Term, 
                                                    exprTerm:qctx.reflect.Term): qctx.reflect.Term = 
       {
          import qctx.reflect._
          import scala.quoted.QuoteContext
          import scala.quoted.Expr

          val valDef = ValDef(oldValDef.symbol, Some(rhs)).asInstanceOf[qctx.reflect.ValDef]
          exprTerm match 
              case Block(stats,last) =>
                    Block(valDef::stats, last)
              case other =>
                    Block(valDef::Nil,other)

       }

       private def buildAppendBlockExpr[A:Type](using qctx: QuoteContext)(oldValDef: qctx.reflect.ValDef, rhs: qctx.reflect.Term, expr:Expr[A]):Expr[A] = 
          import qctx.reflect._
          buildAppendBlock(oldValDef,rhs,expr.unseal).asExprOf[A]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](using qctx: QuoteContext)(
                                      monad: Expr[CpsMonad[F]],
                                      prev: Seq[ExprTreeGen],
                                      oldValDef: qctx.reflect.ValDef,
                                      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev):


       override def isAsync = next.isAsync

       override def syncOrigin(using qctx:QuoteContext): Option[Expr[T]] = next.syncOrigin.map{ n => 
         import qctx.reflect._
         val prevStats: List[Statement] = prev.map(_.extract).toList
         val valDef: Statement = oldValDef.asInstanceOf[qctx.reflect.ValDef]
         val outputTerm = n.unseal match
            case Block(statements, last) => 
                   Block( prevStats ++: (valDef +: statements), last)
            case other => 
                   Block( prevStats ++: List(valDef), other)
         outputTerm.asExprOf[T]
       }
       

       override def fLast(using qctx: QuoteContext) = next.fLast
              
       override def transformed(using qctx: QuoteContext) = {
          import qctx.reflect._

          val valDef = oldValDef.asInstanceOf[qctx.reflect.ValDef]
          val block = next.transformed.unseal match 
             case Block(stats, e) =>
                 Block( prev.map(_.extract) ++: valDef +: stats, e)
             case other =>
                 Block( prev.map(_.extract) ++: List(valDef) , other) 
          block.asExprOf[F[T]]

       }

       override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
          if (exprs.isEmpty)
            this
          else
            ValWrappedCpsExpr[F,T,V](using qctx)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(using qctx: QuoteContext) = 
           ValWrappedCpsExpr(using qctx)(monad, prev, 
                                         oldValDef.asInstanceOf[qctx.reflect.ValDef], 
                                         next.append(e))

       
       def prependPrev(using qctx:QuoteContext)(term: qctx.reflect.Term): qctx.reflect.Term =
          import qctx.reflect._
          if (prev.isEmpty) {
             term
          } else {
             term match
               case Block(stats, expr) =>
                 Block(prev.map(_.extract) ++: stats, expr)
               case other =>
                 Block(prev.toList.map(_.extract) , other)
          }
     

