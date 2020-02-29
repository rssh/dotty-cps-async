package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


object ValDefTransform:



  def fromBlock[F[_]:Type,T:Type](using qctx:QuoteContext)(cpsCtx: TransformationContext[F,T],
                           valDef: qctx.tasty.ValDef): CpsExpr[F,Unit] = {
     import qctx.tasty.{_, given _}
     import cpsCtx._
     val posExpr = Block(List(valDef),Literal(Constant(()))).seal
     val rhs = valDef.rhs.getOrElse(
                  throw MacroError(s"val $valDef without right part in block ", posExpr)
               )
     rhs.seal match {
        case '{ $e: $et } =>
            val cpsRight = Async.nestTransform(e,cpsCtx,"R")
            if (cpsRight.isAsync) {
               RhsFlatMappedCpsExpr(using qctx)(asyncMonad, Seq(),
                                                valDef, cpsRight, CpsExpr.unit(asyncMonad) )
            } else {
               ValWrappedCpsExpr(using qctx)(asyncMonad, Seq(), valDef, 
                                                CpsExpr.unit(asyncMonad) )
            }
        case other =>
            throw MacroError(s"Can't concretize type of right-part $rhs ", posExpr)
     }

     
  }


  class RhsFlatMappedCpsExpr[F[_]:Type, T:Type, V:Type](using qctx:QuoteContext)
                                     (monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: qctx.tasty.ValDef,
                                      cpsRhs: CpsExpr[F,V],
                                      next: CpsExpr[F,T]
                                     )
                                    extends AsyncCpsExpr[F,T](monad, prev) {

       override def fLast(using qctx: QuoteContext) = 
          import qctx.tasty.{_, given _}

          def appendBlockExpr[A](rhs: qctx.tasty.Term, expr: Expr[A]):Expr[A] =
                buildAppendBlockExpr(oldValDef.asInstanceOf[qctx.tasty.ValDef],
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

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
          RhsFlatMappedCpsExpr(using qctx)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:quoted.Type](e: CpsExpr[F,A])(using qtcx: QuoteContext) = 
          RhsFlatMappedCpsExpr(using qctx)(monad,prev,oldValDef,cpsRhs,next.append(e))
                                                          
             
       private def buildAppendBlock(using qctx:QuoteContext)(
                      oldValDef: qctx.tasty.ValDef, rhs:qctx.tasty.Term, 
                                                    exprTerm:qctx.tasty.Term): qctx.tasty.Term = 
       {
          import qctx.tasty.{_, given _}
          import scala.internal.quoted.showName
          import scala.quoted.QuoteContext
          import scala.quoted.Expr

          val valDef = ValDef(oldValDef.symbol, Some(rhs)).asInstanceOf[qctx.tasty.ValDef]
          exprTerm match 
              case Block(stats,last) =>
                     Block(valDef::stats, last)
              case other =>
                    Block(valDef::Nil,other)

       }

       private def buildAppendBlockExpr[A](using qctx: QuoteContext)(oldValDef: qctx.tasty.ValDef, rhs: qctx.tasty.Term, expr:Expr[A]):Expr[A] = 
          import qctx.tasty.{_, given _}
          buildAppendBlock(oldValDef,rhs,expr.unseal).seal.asInstanceOf[Expr[A]]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](using qctx: QuoteContext)(
                                      monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: qctx.tasty.ValDef,
				      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev):


       override def isAsync = next.isAsync

       override def fLast(using qctx: QuoteContext) = next.fLast
              
       override def transformed(using qctx: QuoteContext) = {
          import qctx.tasty.{_, given _}

          // not worked due https://github.com/lampepfl/dotty/issues/8168
          val valDef = oldValDef.asInstanceOf[qctx.tasty.ValDef]
          val block = next.transformed.unseal match 
             case Block(stats, e) =>
                 Block( prev.map(_.unseal) ++: valDef +: stats, e)
             case other =>
                 Block( prev.map(_.unseal) ++: List(valDef) , other) 
          block.seal.asInstanceOf[Expr[F[T]]]

          //val r = prependPrev(genBlock(oldValDef.asInstanceOf[qctx.tasty.ValDef], 
          //                    next.transformed.unseal))
          //r.seal.asInstanceOf[Expr[F[T]]]
       }

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           ValWrappedCpsExpr[F,T,V](using qctx)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(using qctx: QuoteContext) = 
           ValWrappedCpsExpr(using qctx)(monad, prev, 
                                         oldValDef.asInstanceOf[qctx.tasty.ValDef], 
                                         next.append(e))

       def genBlock(using qctx:QuoteContext)(oldValDef: qctx.tasty.ValDef, nextTerm: qctx.tasty.Term): qctx.tasty.Term =
            import qctx.tasty.{_, given _}
            import scala.internal.quoted.showName
            import scala.quoted.QuoteContext
            import scala.quoted.Expr

            // workarrround against https://github.com/lampepfl/dotty/issues/8045
            def substituteIdent(nextTerm: Term, v: Symbol, newIdent:Ident):Term =
                  TransformUtil.substituteIdent(nextTerm,v,newIdent)

            // TODO: add to scala Reflection ValDefOps something like isVal
            // for now - use quote matching to determinate
            val valDefExpr = Block(List(oldValDef),Literal(Constant(()))).seal
            valDefExpr match 
              case '{ val $v:$tv = $y } =>
                 val name: String = v.name
                 '{ 
                   @showName(${Expr(name)})
                   val x:$tv = $y
                   ${substituteIdent(nextTerm,oldValDef.symbol,'x.unseal.asInstanceOf[Ident]).seal}
                  }.unseal
              case '{ var $x:$tx = $y } =>
                  val name: String = x.name
                  '{ 
                    @showName(${Expr(name)})
                    var x:$tx = $y
                    ${substituteIdent(nextTerm,oldValDef.symbol,'x.unseal.asInstanceOf[Ident]).seal}
                   }.unseal
              case other =>
                   throw MacroError(s"Invalid ValDef $oldValDef", valDefExpr) 
                

       def prependPrev(using qctx:QuoteContext)(term: qctx.tasty.Term): qctx.tasty.Term =
          import qctx.tasty.{_, given _}
          if (prev.isEmpty) {
             term
          } else {
             term match
               case Block(stats, expr) =>
                 Block(prev.map(_.unseal) ++: stats, expr)
               case other =>
                 Block(prev.map(_.unseal).toList, other)
          }
     

