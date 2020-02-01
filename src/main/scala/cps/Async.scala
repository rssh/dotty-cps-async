package cps

import scala.quoted._
import scala.quoted.matching._

trait CB[T] 

object CBM {

   def pure[T](value:T): CB[T] = ??? 

}

trait CpsExpr[T:Type](prev: Seq[Expr[_]])

  def transformed(given QuoteContext): Expr[CB[T]] 

  def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T]

  def append[A:Type](chunk: CpsExpr[A])(given QuoteContext): CpsExpr[A]



case class GenericSyncCpsExpr[T:Type](
                             prev: Seq[Expr[_]],
                             lastExpr: Expr[T]) extends CpsExpr[T](prev)

       def transformed(given QuoteContext): Expr[CB[T]] =
          Expr.block(prev.toList, '{ CBM.pure(${lastExpr}) })

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T] =
           copy(prev = exprs ++: prev)

       override def append[A:Type](e: CpsExpr[A])(given QuoteContext) = 
           e.prependExprs(Seq(lastExpr)).prependExprs(prev)

           

class ValWrappedCpsExpr[T:Type, V:Type](
                                      prev: Seq[Expr[_]],
                                      oldValDefBlock: Expr[Unit],
				      next: CpsExpr[T] ) extends CpsExpr[T](prev)

       override def transformed(given qctx: QuoteContext) = 
           import qctx.tasty.{_,given}
           val oldValDef = extractValDef(oldValDefBlock)
           val block = Block( prev.map(_.unseal) ++: List(oldValDef), next.transformed.unseal) 
           block.seal.asInstanceOf[Expr[CB[T]]]

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T] =
           ValWrappedCpsExpr[T,V](exprs ++: prev, oldValDefBlock, next)

       override def append[A:quoted.Type](e:CpsExpr[A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(prev, oldValDefBlock,  next.append(e))
                                       
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
 

object CpsExpr

   def sync[T:Type](f: Expr[T]): CpsExpr[T] =
     GenericSyncCpsExpr[T](Seq(), f) 
         
   def unit()(given QuoteContext) =
     GenericSyncCpsExpr[Unit](Seq(), '{}) 


object Async {

  inline def transform[T](expr: =>T): CB[T] =
    ${ Async.transformImpl[T]('expr) } 

  def transformImpl[T:Type](f: Expr[T])(given qctx: QuoteContext): Expr[CB[T]] = 
    import qctx.tasty.{_,given}
    val r = rootTransform[T](f,false).transformed
    println(s"transformed value: ${r.show}")
    r

  def rootTransform[T:Type](f: Expr[T], inBlock: Boolean)(
                                           given qctx: QuoteContext): CpsExpr[T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     import util._
     f match 
         case Const(c) =>  
              CpsExpr.sync(f)
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   CpsExpr.sync(f)
                case Block(prevs,last) =>
                   val rPrevs = prevs.map{
                         case d: Definition =>
                            d match {
                               case v@ValDef(vName,vtt,optRhs) =>
                                 optRhs match {
                                   case Some(rhs) =>
                                     val patternExpr = Block(List(v),Literal(Constant(()))).seal
                                     val patternExprUnit = patternExpr.asInstanceOf[Expr[Unit]]
                                     ValWrappedCpsExpr(Seq(), patternExprUnit, CpsExpr.unit() )
                                }
                            }
                         case t: Term =>
                            t.seal match
                              case '{ $p:$tp } =>
                                Async.rootTransform(p, true)
                              case other =>
                                printf(other.show)
                                throw IllegalStateException(s"can't handle statement in block: $other")
                   }
                   val rLast = Async.rootTransform[T](last.seal.asInstanceOf[Expr[T]],true)
                   rPrevs.foldRight(rLast)((e,s) => e.append(s))
                case Ident(name) =>
                   CpsExpr.sync(f)
                case _ =>
                   printf("fTree:"+fTree)
                   throw IllegalStateException(s"language construction is not supported: ${fTree}")
             }
   

}


