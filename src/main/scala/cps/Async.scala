package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._


trait AsyncMonad[F[_]] {

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

}

trait ComputationBound[T] {

}


implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ??? 

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = ??? 

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = ??? 

}



trait CpsExpr[F[_]:Type,T:Type](monad:Expr[AsyncMonad[F]], prev: Seq[Expr[_]])

  def isAsync: Boolean

  def fLast(given QuoteContext): Expr[F[T]]

  def transformed(given QuoteContext): Expr[F[T]] =
     if (prev.isEmpty)
         fLast
     else
         Expr.block(prev.toList,fLast)

  def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T]

  def append[A:Type](chunk: CpsExpr[F,A])(given QuoteContext): CpsExpr[F,A]

  def syncOrigin(given QuoteContext): Option[Expr[T]]  
  
  def asyncMonad: Expr[AsyncMonad[F]] = monad

  def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[F,A] = ???

  def flatMap[A:Type](f: Expr[T => F[A]])(given QuoteContext): CpsExpr[F,A] = ???

  def flatMapIgnore[A:Type](t: Expr[F[A]])(given QuoteContext): CpsExpr[F,A] =
         flatMap( '{ _ => $t } )



abstract class SyncCpsExpr[F[_]:Type, T: Type](dm: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]]) extends CpsExpr[F,T](dm, prev) 
 
     override def isAsync = false

     def last(given QuoteContext): Expr[T]

     override def fLast(given QuoteContext): Expr[F[T]] =
          '{  ${dm}.pure(${last}) }

     override def syncOrigin(given QuoteContext): Option[Expr[T]] = Some(
         if prev.isEmpty then
            last
         else
            Expr.block(prev.toList,last)
     )

                  

case class GenericSyncCpsExpr[F[_]:Type,T:Type](
                             dm: Expr[AsyncMonad[F]],
                             prev: Seq[Expr[_]],
                             lastExpr: Expr[T]) extends SyncCpsExpr[F,T](dm, prev)

       
       def last(given QuoteContext): Expr[T] = lastExpr

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

       override def append[A:Type](e: CpsExpr[F,A])(given QuoteContext) = 
           e.prependExprs(Seq(last)).prependExprs(prev)

       override def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[F,A] =
           copy(lastExpr = Expr.betaReduce(f)(lastExpr) )

       override def flatMap[A:Type](f: Expr[T => F[A]])(given QuoteContext): CpsExpr[F,A] =
            GenericAsyncCpsExpr[F,A](dm, prev, '{ $dm.flatMap($dm.pure($last))($f) } ) 
                           

       override def flatMapIgnore[A:Type](t: Expr[F[A]])(given QuoteContext): CpsExpr[F,A] =
            GenericAsyncCpsExpr(dm, prev, '{ ${dm}.flatMap($dm.pure($last))(_ => $t) } )
                         
           

abstract class AsyncCpsExpr[F[_]:Type,T:Type](
                              dm: Expr[AsyncMonad[F]],
                              prev: Seq[Expr[_]]
                            ) extends CpsExpr[F,T](dm, prev)

       override def isAsync = true

       override def append[A:Type](e: CpsExpr[F,A])(given QuoteContext): CpsExpr[F,A] = 
           flatMapIgnore(e.transformed)

       override def syncOrigin(given QuoteContext): Option[Expr[T]] = None



case class GenericAsyncCpsExpr[F[_]:Type,T:Type](
                            dm: Expr[AsyncMonad[F]],
                            prev: Seq[Expr[_]],
                            fLastExpr: Expr[F[T]]) extends AsyncCpsExpr[F,T](dm, prev) {

    override def fLast(given QuoteContext): Expr[F[T]] = fLastExpr

    override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           copy(prev = exprs ++: prev)

    override def map[A:Type](f: Expr[T => A])(given QuoteContext): CpsExpr[F,A] = ???

    override def flatMap[A:Type](f: Expr[T => F[A]])(given QuoteContext): CpsExpr[F,A] = ???

    override def flatMapIgnore[A:Type](t: Expr[F[A]])(given QuoteContext): CpsExpr[F,A] = ???

}


class ValWrappedCpsExpr[F[_]:Type, T:Type, V:Type](
                                      monad: Expr[AsyncMonad[F]],
                                      prev: Seq[Expr[_]],
                                      oldValDefBlock: Expr[Unit],
				      next: CpsExpr[F,T] ) extends AsyncCpsExpr[F,T](monad,prev)


       override def isAsync = next.isAsync

       override def fLast(given qctx: QuoteContext) = next.fLast
              
       override def transformed(given qctx: QuoteContext) = 
           import qctx.tasty.{_,given}
           val oldValDef = extractValDef(oldValDefBlock)
           val block = next.transformed.unseal match 
             //case Block(stats, e) =>
             //    Block( prev.map(_.unseal) ++: oldValDef +: stats, e)
             case other =>
                 Block( prev.map(_.unseal) ++: List(oldValDef), other) 
           block.seal.asInstanceOf[Expr[F[T]]]

       override def prependExprs(exprs: Seq[Expr[_]]): CpsExpr[F,T] =
           ValWrappedCpsExpr[F,T,V](monad, exprs ++: prev, oldValDefBlock, next)

       override def append[A:quoted.Type](e:CpsExpr[F,A])(given qctx: QuoteContext) = 
           ValWrappedCpsExpr(monad, prev, 
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


   def sync[F[_]:Type,T:Type](dm: Expr[AsyncMonad[F]], f: Expr[T]): CpsExpr[F,T] =
     GenericSyncCpsExpr[F,T](dm, Seq(), f) 
         
   def async[F[_]:Type,T:Type](dm: Expr[AsyncMonad[F]], f: Expr[F[T]]): CpsExpr[F,T] = 
     GenericAsyncCpsExpr[F,T](dm, Seq(), f) 

   def unit[F[_]:Type](dm: Expr[AsyncMonad[F]])(given QuoteContext) =
     GenericSyncCpsExpr[F,Unit](dm, Seq(), '{}) 

   def wrap[F[_]:Type, T:Type](internal:CpsExpr[F,T])(given QuoteContext): CpsExpr[F,T] =
      internal.syncOrigin match 
         case Some(origin) => sync(internal.asyncMonad, origin)
         case None => async(internal.asyncMonad, internal.transformed)


object Async {

  inline def transform[F[_], T](expr: =>T): F[T] =
    ${ Async.transformImpl[F,T]('expr) } 

  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(given qctx: QuoteContext): Expr[F[T]] = 
    import qctx.tasty.{_,given}
    summonExpr[AsyncMonad[F]] match 
        case Some(dm) => 
             println(s"before transformed: ${f.show}")
             //println(s"value: ${f.unseal}")
             val r = rootTransform[F,T](f,dm,false).transformed
             println(s"transformed value: ${r.show}")
             //println(s"transformed tree: ${r.unseal}")
             r
        case None => 
             val ft = summon[quoted.Type[F]]
             throw new IllegalStateException(s"Can't find async monad for ${ft.show}")


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]], inBlock: Boolean)(
                                           given qctx: QuoteContext): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     import util._
     f match 
         case Const(c) =>  
              CpsExpr.sync(dm, f)
         case '{ val $x:$tx = $y } if inBlock => 
              val cpsRight = Async.rootTransform(y,dm,false)
              val unitPatternCode = f.asInstanceOf[Expr[Unit]]
              val cpsBuild = ValWrappedCpsExpr(dm, Seq(), unitPatternCode,
                                                     CpsExpr.unit(dm) )
              cpsBuild.asInstanceOf[CpsExpr[F,T]]
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   CpsExpr.sync(dm, f)
                case Block(prevs,last) =>
                   val rPrevs = prevs.map{
                         case d: Definition =>
                            d match {
                               case v@ValDef(vName,vtt,optRhs) =>
                                 optRhs match {
                                   case Some(rhs) =>
                                     val patternExpr = Block(List(v),Literal(Constant(()))).seal
                                     val patternExprUnit = patternExpr.asInstanceOf[Expr[Unit]]
                                     ValWrappedCpsExpr(dm, Seq(), patternExprUnit, CpsExpr.unit(dm) )
                                }
                            }
                         case t: Term =>
                            t.seal match
                              case '{ $p:$tp } =>
                                Async.rootTransform(p, dm, true)
                              case other =>
                                printf(other.show)
                                throw IllegalStateException(s"can't handle statement in block: $other")
                   }
                   val rLast = Async.rootTransform[F,T](last.seal.asInstanceOf[Expr[T]],dm,true)
                   val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
                   CpsExpr.wrap(blockResult)
                case Ident(name) =>
                   CpsExpr.sync(dm, f)
                case _ =>
                   printf("fTree:"+fTree)
                   throw IllegalStateException(s"language construction is not supported: ${fTree}")
             }
   
   inline def testm():ComputationBound[Int] =
     ${ testImpl }
     
   def testImpl(given qctx: QuoteContext) = {
     Async.transformImpl[ComputationBound,Int]('{
         val x1 = 3
         val x2 = 4
         x1 + x2
     })
   }

}
