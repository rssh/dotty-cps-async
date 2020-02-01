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
      //val oldValDef = extractValDef(unitPatternCode)
      val cpsBuild = ValWrappedCpsExpr[F,Unit,TX](asyncMonad, Seq(), unitPatternCode, 
                                                     CpsExpr.unit(asyncMonad) )
      cpsBuild.asInstanceOf[CpsExpr[F,T]]


  class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](
                                      monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDef: Expr[Unit],
				      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev)


       override def isAsync = next.isAsync

       override def fLast(given qctx: QuoteContext) = next.fLast
              
       override def transformed(given qctx: QuoteContext) = 
           import qctx.tasty.{_,given}
           val block = next.transformed.unseal match 
             //case Block(stats, e) =>
             //    Block( prev.map(_.unseal) ++: oldValDef +: stats, e)
             case other =>
                 //Block( prev.map(_.unseal) ++: List(oldValDef), other) 
                 Block( prev.map(_.unseal) ++: List(oldValDef.unseal), other) 
           block.seal.asInstanceOf[Expr[F[T]]]

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           ValWrappedCpsExpr[F,T,V](monad, exprs ++: prev, oldValDef, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(monad, prev, 
                                         oldValDef, 
                                         next.append(e))


     


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
 
