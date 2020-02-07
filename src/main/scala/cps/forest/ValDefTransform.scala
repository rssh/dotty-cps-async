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
            val cpsRight = Async.rootTransform(e,asyncMonad,false)
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

       import qctx.tasty.{_, given}

       override def fLast(given qctx: QuoteContext) = 
          next.syncOrigin match 
            case Some(nextOrigin) =>
             '{
               ${monad}.map(${cpsRhs.transformed})((v:V) => 
                          ${buildAppendBlockExpr(oldValDef, 'v.unseal, nextOrigin)}) 
              }
            case  None =>
             '{
               ${monad}.flatMap(${cpsRhs.transformed})((v:V)=>
                          ${buildAppendBlockExpr(oldValDef, 'v.unseal, next.transformed)}) 
             }

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
          RhsFlatMappedCpsExpr(given qctx)(monad, exprs ++: prev,oldValDef,cpsRhs,next)


       override def append[A:quoted.Type](e: CpsExpr[F,A])(given qtcx: QuoteContext) = 
          RhsFlatMappedCpsExpr(given qctx)(monad,prev,oldValDef,cpsRhs,next.append(e))
                                                          
             
       private def buildAppendBlock(oldValDef: ValDef, rhs:Term, exprTerm:Term):Term = 
          val valDef = ValDef(oldValDef.symbol, Some(rhs))
          exprTerm match 
              case Block(stats,last) =>
                     Block(valDef::stats, last)
              case other =>
                    Block(valDef::Nil,other)

       private def buildAppendBlockExpr[A](oldValDef: ValDef, rhs:Term, expr:Expr[A]):Expr[A] = 
                 buildAppendBlock(oldValDef,rhs,expr.unseal).seal.asInstanceOf[Expr[A]]

  }

  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](given qctx: QuoteContext)(
                                      monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: qctx.tasty.ValDef,
				      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev)

       import qctx.tasty.{_, given}

       override def isAsync = next.isAsync

       override def fLast(given qctx: QuoteContext) = next.fLast
              
       override def transformed(given qctx: QuoteContext) = 
           val block = next.transformed.unseal match 
             case Block(stats, e) =>
                 Block( prev.map(_.unseal) ++: oldValDef +: stats, e)
             case other =>
                 Block( prev.map(_.unseal) ++: List(oldValDef) , other) 
           block.seal.asInstanceOf[Expr[F[T]]]

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           ValWrappedCpsExpr[F,T,V](given qctx)(monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(given qctx)(monad, prev, 
                                         oldValDef.asInstanceOf[qctx.tasty.ValDef], 
                                         next.append(e))


     
  def newValDef(given qctx: QuoteContext)(oldValDef: qctx.tasty.ValDef, name: String, newRhs: qctx.tasty.Term): qctx.tasty.ValDef = {
         import qctx.tasty.{_,given}
         ValDef.copy(oldValDef)(name,oldValDef.tpt,Some(newRhs))
  }

  def valDefBlock(given qctx:QuoteContext)(v:qctx.tasty.ValDef):Expr[Unit] = {
    import qctx.tasty.{_,given}
    Block(List(v),Literal(Constant(()))).seal.asInstanceOf[Expr[Unit]]
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


