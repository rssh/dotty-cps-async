package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


object ValDefTransform

     //    case '{ val $x:$tx = $y } => 
  def run[F[_]:Type,T:Type,TX:Type](transformationContext: TransformationContext[F,T],
                  x:Sym[TX],tx:Type[TX],y:Expr[TX])(given qctx: QuoteContext) =
      import transformationContext._
      import qctx.tasty.{_, given}
      import util._
      val cpsRight = Async.rootTransform[F,TX](y,asyncMonad,false)
      val unitPatternCode = patternCode.asInstanceOf[Expr[Unit]]
      val oldValDef = extractValDef(unitPatternCode)
      if (cpsRight.isAsync) 
         val cpsBuild = RhsFlatMappedCpsExpr[F,Unit,TX](given qctx)(asyncMonad, Seq(),
                                                   oldValDef, cpsRight, CpsExpr.unit(asyncMonad) )
         cpsBuild.asInstanceOf[CpsExpr[F,T]]
      else
         val cpsBuild = ValWrappedCpsExpr[F,Unit,TX](given qctx)(asyncMonad, Seq(), oldValDef, 
                                                     CpsExpr.unit(asyncMonad) )
         cpsBuild.asInstanceOf[CpsExpr[F,T]]


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

  def extractValDef(given qctx:QuoteContext)(blockExpr:Expr[Unit]): qctx.tasty.ValDef = {
    import qctx.tasty.{_,given}
    blockExpr.unseal match {
      case Block(stats,last) =>
        stats.head match {
          case v: ValDef => v
          case _ => qctx.error("Block with ValDef as first statement expected",blockExpr)
                  ???
        }
      case Inlined(call,binding,body) => extractValDef(body.seal.asInstanceOf[Expr[Unit]])
      case _ => qctx.error("Block expected",blockExpr)
                ??? 
    }
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


