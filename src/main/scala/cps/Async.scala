package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps.forest._
import cps.misc._


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

  //def pure[A:Type](t: Expr[A])(given QuoteContext): CpsExpr[F,A] = 
  //                     CpsExpr.sync(monad, t)

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
    try
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
             throw MacroError(s"Can't find async monad for ${ft.show}", f)
    catch
      case ex: MacroError =>
           qctx.error(ex.msg, ex.posExpr)
           '{???}


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]], inBlock: Boolean)(
                                           given qctx: QuoteContext): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     import util._
     val cpsCtx = TransformationContext[F,T](f,tType,dm, inBlock)
     f match 
         case Const(c) =>  
              CpsExpr.sync(cpsCtx.asyncMonad, cpsCtx.patternCode)
         case '{ val $x:$tx = $y } if inBlock => 
                            ValDefTransform.run(cpsCtx, x, tx, y)
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   CpsExpr.sync(cpsCtx.asyncMonad, cpsCtx.patternCode)
                case Block(prevs,last) =>
                   BlockTransform(cpsCtx).run(prevs,last)
                case Ident(name) =>
                   CpsExpr.sync(cpsCtx.asyncMonad, cpsCtx.patternCode)
                case _ =>
                   printf("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
             }
     

}
