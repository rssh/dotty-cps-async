package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


object ValDefTransform


  def fromBlock[F[_]:Type](given qctx:QuoteContext)(cpsCtx: TransformationContext[F,_],
                           valDef: qctx.tasty.ValDef): CpsExpr[F,Unit] = {
     import qctx.tasty.{_, given}
     import cpsCtx._
     val posExpr = Block(List(valDef),Literal(Constant(()))).seal
     val rhs = valDef.rhs.getOrElse(
                  throw MacroError(s"val $valDef without right part in block ", posExpr)
               )
     rhs.seal match {
        case '{ $e: $et } =>
            val cpsRight = Async.rootTransform(e,asyncMonad,exprMarker+"R")
            if (cpsRight.isAsync) {
               RhsFlatMappedCpsExpr(given qctx)(asyncMonad, Seq(),
                                                valDef, cpsRight, CpsExpr.unit(asyncMonad) )
            } else {
               ValWrappedCpsExpr(given qctx)(asyncMonad, Seq(), valDef, 
                                                CpsExpr.unit(asyncMonad) )
            }
        case other =>
            throw MacroError(s"Can't concretize type of right-part $rhs ", posExpr)
     }

     
  }


  class RhsFlatMappedCpsExpr[F[_]:Type, T:Type, V:Type](given qctx:QuoteContext)
                                     (monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: qctx.tasty.ValDef,
                                      cpsRhs: CpsExpr[F,V],
                                      next: CpsExpr[F,T]
                                     )
                                    extends AsyncCpsExpr[F,T](monad, prev) {

       override def fLast(given qctx: QuoteContext) = 
          import qctx.tasty.{_, given}

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
          RhsFlatMappedCpsExpr(given qctx)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:quoted.Type](e: CpsExpr[F,A])(given qtcx: QuoteContext) = 
          RhsFlatMappedCpsExpr(given qctx)(monad,prev,oldValDef,cpsRhs,next.append(e))
                                                          
             
       private def buildAppendBlock(given qctx:QuoteContext)(
                      oldValDef: qctx.tasty.ValDef, rhs:qctx.tasty.Term, 
                                                    exprTerm:qctx.tasty.Term): qctx.tasty.Term = 
       {
          import qctx.tasty.{_, given}
          import scala.internal.quoted.showName
          import scala.quoted.QuoteContext
          import scala.quoted.Expr

          // not worked due https://github.com/lampepfl/dotty/issues/8168
          //
          //val valDef = ValDef(oldValDef.symbol, Some(rhs))
          //exprTerm match 
          //    case Block(stats,last) =>
          //           Block(valDef::stats, last)
          //    case other =>
          //          Block(valDef::Nil,other)

          // workarrround against https://github.com/lampepfl/dotty/issues/8045
          def substituteIdent(nextTerm: Term, v: Symbol, newIdent:Ident):Term =
                  TransformUtil.substituteIdent(nextTerm,v,newIdent)


          //val valDef = ValDef(oldValDef.symbol, Some(rhs))
          val oldValDefExpr = Block(List(oldValDef),Literal(Constant(()))).seal
          oldValDefExpr match 
            case '{ val $x:$tx = $y } =>
                val name: String = x.name 
                '{ 
                   @showName(${Expr(name)})
                   val x:V = ${rhs.seal.asInstanceOf[Expr[V]]}
                   ${substituteIdent(exprTerm,oldValDef.symbol,'x.unseal.asInstanceOf[Ident]).seal}
                }.unseal
            case '{ var $x:$tx = $y } =>
                val name: String = x.name 
                '{ 
                   @showName(${Expr(name)})
                   var x:V = ${rhs.seal.asInstanceOf[Expr[V]]}
                   ${substituteIdent(exprTerm,oldValDef.symbol,'x.unseal.asInstanceOf[Ident]).seal}
                }.unseal

       }

       private def buildAppendBlockExpr[A](given qctx: QuoteContext)(oldValDef: qctx.tasty.ValDef, rhs: qctx.tasty.Term, expr:Expr[A]):Expr[A] = 
          import qctx.tasty.{_, given}
          buildAppendBlock(oldValDef,rhs,expr.unseal).seal.asInstanceOf[Expr[A]]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](given qctx: QuoteContext)(
                                      monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: qctx.tasty.ValDef,
				      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev)


       override def isAsync = next.isAsync

       override def fLast(given qctx: QuoteContext) = next.fLast
              
       override def transformed(given qctx: QuoteContext) = {
          import qctx.tasty.{_, given}

          // not worked due https://github.com/lampepfl/dotty/issues/8168
          //  val block = next.transformed.unseal match 
          //   case Block(stats, e) =>
          //       Block( prev.map(_.unseal) ++: oldValDef +: stats, e)
          //   case other =>
          //       Block( prev.map(_.unseal) ++: List(oldValDef) , other) 
          // block.seal.asInstanceOf[Expr[F[T]]]

          val r = prependPrev(genBlock(oldValDef.asInstanceOf[qctx.tasty.ValDef], 
                              next.transformed.unseal))
          r.seal.asInstanceOf[Expr[F[T]]]
       }

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           ValWrappedCpsExpr[F,T,V](given qctx)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(given qctx)(monad, prev, 
                                         oldValDef.asInstanceOf[qctx.tasty.ValDef], 
                                         next.append(e))

       def genBlock(given qctx:QuoteContext)(oldValDef: qctx.tasty.ValDef, nextTerm: qctx.tasty.Term): qctx.tasty.Term =
            import qctx.tasty.{_, given}
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
                

       def prependPrev(given qctx:QuoteContext)(term: qctx.tasty.Term): qctx.tasty.Term =
          import qctx.tasty.{_, given}
          if (prev.isEmpty) {
             term
          } else {
             term match
               case Block(stats, expr) =>
                 Block(prev.map(_.unseal) ++: stats, expr)
               case other =>
                 Block(prev.map(_.unseal).toList, other)
          }
     

 
  // substitute identifier with the same part.


  def substituteIdent(given qctx:QuoteContext)(tree: qctx.tasty.Term, 
                           origin: qctx.tasty.Symbol, 
                           newIdent: qctx.tasty.Ident): qctx.tasty.Term =
     import qctx.tasty.{_,given}
     import qctx.tasty._
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(given ctx: Context):Term =
          tree match 
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newIdent
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


