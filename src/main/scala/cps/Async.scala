package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._


trait CB[T] {

}


object CBM {

   def pure[T](value:T): CB[T] = ??? 

   def map[A,B](fa:CB[A])(f: A=>B):CB[B] = ??? 

   def flatMap[A,B](fa:CB[A])(f: A=>CB[B]):CB[B] = ??? 

}



trait CpsExpr[T:Type](prev: Seq[Expr[_]])

  def isAsync: Boolean

  def fLast(given QuoteContext): Expr[CB[T]]

  def transformed(given QuoteContext): Expr[CB[T]] =
     if (prev.isEmpty)
         fLast
     else
         Expr.block(prev.toList,fLast)

  def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T]

  def append[A:Type](chunk: CpsExpr[A])(given QuoteContext): CpsExpr[A]

  def syncOrigin(given QuoteContext): Option[Expr[T]]  
  
  def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[A] = ???

  def flatMap[A:Type](f: Expr[T => CB[A]])(given QuoteContext): CpsExpr[A] = ???



abstract class SyncCpsExpr[T: Type](
                                      prev: Seq[Expr[_]]) extends CpsExpr[T](prev) 
 
     override def isAsync = false

     def last(given QuoteContext): Expr[T]

     override def fLast(given QuoteContext): Expr[CB[T]] =
          '{  CBM.pure(${last}) }

     override def syncOrigin(given QuoteContext): Option[Expr[T]] = Some(
         if prev.isEmpty then
            last
         else
            Expr.block(prev.toList,last)
     )

                  

case class GenericSyncCpsExpr[T:Type](
                             prev: Seq[Expr[_]],
                             lastExpr: Expr[T]) extends SyncCpsExpr[T](prev)

       
       def last(given QuoteContext): Expr[T] = lastExpr

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T] =
           copy(prev = exprs ++: prev)

       override def append[A:Type](e: CpsExpr[A])(given QuoteContext) = 
           e.prependExprs(Seq(last)).prependExprs(prev)

       override def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[A] =
           copy(lastExpr = Expr.betaReduce(f)(lastExpr) )

       override def flatMap[A:Type](f: Expr[T => CB[A]])(given QuoteContext): CpsExpr[A] =
            GenericAsyncCpsExpr[A](prev, '{ CBM.flatMap(CBM.pure($last))($f) } ) 
                           

           

abstract class AsyncCpsExpr[T:Type](
                              prev: Seq[Expr[_]]
                            ) extends CpsExpr[T](prev)

       override def isAsync = true

       override def append[A:Type](e: CpsExpr[A])(given QuoteContext): CpsExpr[A] 

       override def syncOrigin(given QuoteContext): Option[Expr[T]] = None



case class GenericAsyncCpsExpr[T:Type](
                            prev: Seq[Expr[_]],
                            fLastExpr: Expr[CB[T]]) extends AsyncCpsExpr[T](prev) {

    override def fLast(given QuoteContext): Expr[CB[T]] = fLastExpr

    override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T] =
           copy(prev = exprs ++: prev)

    override def append[A:Type](e: CpsExpr[A])(given QuoteContext): CpsExpr[A] = ???

    override def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[A] = ???

    override def flatMap[A:Type](f: Expr[T => CB[A]])(given QuoteContext): CpsExpr[A] = ???

}


class ValWrappedCpsExpr[T:Type, V:Type](
                                      prev: Seq[Expr[_]],
                                      oldValDefBlock: Expr[Unit],
				      next: CpsExpr[T] ) extends AsyncCpsExpr[T](prev)


       override def isAsync = next.isAsync

       override def fLast(given qctx: QuoteContext) = next.fLast
              
       override def transformed(given qctx: QuoteContext) = 
           import qctx.tasty.{_,given}
           val oldValDef = extractValDef(oldValDefBlock)
           val block = 
                 Block( prev.map(_.unseal) ++: List(oldValDef), next.transformed.unseal) 
           block.seal.asInstanceOf[Expr[CB[T]]]

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[T] =
           ValWrappedCpsExpr[T,V](exprs ++: prev, oldValDefBlock, next)

       override def append[A:quoted.Type](e:CpsExpr[A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(prev, 
                                         oldValDefBlock,
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
 


object CpsExpr


   def sync[T:Type](f: Expr[T]): CpsExpr[T] =
     GenericSyncCpsExpr[T](Seq(), f) 
         
   def async[T:Type](f: Expr[CB[T]]): CpsExpr[T] = 
     GenericAsyncCpsExpr[T](Seq(), f) 

   def unit()(given QuoteContext) =
     GenericSyncCpsExpr[Unit](Seq(), '{}) 

   def wrap[T:Type](internal:CpsExpr[T])(given QuoteContext): CpsExpr[T] =
      internal.syncOrigin match 
         case Some(origin) => sync(origin)
         case None => async(internal.transformed)


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
         case '{ val $x:$tx = $y } if inBlock => 
              val cpsRight = Async.rootTransform(y,false)
              val unitPatternCode = f.asInstanceOf[Expr[Unit]]
              val cpsBuild = ValWrappedCpsExpr(Seq(), unitPatternCode,
                                                     CpsExpr.unit() )
              cpsBuild.asInstanceOf[CpsExpr[T]]
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
                   val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
                   CpsExpr.wrap(blockResult)
                case Ident(name) =>
                   CpsExpr.sync(f)
                case _ =>
                   printf("fTree:"+fTree)
                   throw IllegalStateException(s"language construction is not supported: ${fTree}")
             }
   
   inline def testm():CB[Int] =
     ${ testImpl }
     
   def testImpl(given qctx: QuoteContext):Expr[CB[Int]] = {
     Async.transformImpl[Int]('{
         val x1 = 3
         val x2 = 4
         x1 + x2
     })
   }

}


