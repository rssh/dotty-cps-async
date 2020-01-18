package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import scala.util.Try

trait AsyncMonad[F[_]] {

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

}

case class CpsChunk[F[_],T](prev: Seq[Expr[_]], last:Expr[F[T]]) 

    def toExpr(given QuoteContext): Expr[F[T]] =
      val nLast = last
      if (prev.isEmpty)
        nLast
      else
        Expr.block(prev.toList,nLast)

    def insertPrev[A](p: Expr[A]): CpsChunk[F,T] =
      CpsChunk(p +: prev, last) 
  

trait CpsChunkBuilder[F[_]:Type,T:Type](monad:Expr[AsyncMonad[F]])

  def create(): CpsChunk[F,T]

  def append[A:Type](chunk: CpsChunk[F,A]): CpsChunk[F,A]
  
  protected def fromFExpr(f: Expr[F[T]]): CpsChunk[F,T] =
          CpsChunk(Seq(),f)

  def pure[A:Type](t: Expr[A])(given QuoteContext): CpsChunk[F,A] = 
                           CpsChunk[F,A](Seq(),'{ ${monad}.pure(${t}) })

  def map[A:Type](t: Expr[T => A])(given QuoteContext): CpsChunk[F,A] =
                           CpsChunk[F,A](Seq(),'{ ${monad}.map(${create().toExpr})(${t}) })


  def flatMap[A:Type](t: Expr[T => F[A]])(given QuoteContext): CpsChunk[F,A] =
                 CpsChunk[F,A](Seq(), 
                      '{ ${monad}.flatMap(${create().toExpr})(${t}) }
                 )

  def flatMapIgnore[A:Type](t: Expr[F[A]])(given QuoteContext): CpsChunk[F,A] =
           CpsChunk[F,A](Seq(), 
                 '{ ${monad}.flatMap(${create().toExpr})(_ => ${t}) }
           )



object CpsChunkBuilder 

   def sync[F[_]:Type,T:Type](f:Expr[T], dm: Expr[AsyncMonad[F]])(given QuoteContext):CpsChunkBuilder[F,T] =
     new CpsChunkBuilder[F,T](dm) {
        override def create() = fromFExpr('{ ${dm}.pure($f) })
        override def append[A:Type](e: CpsChunk[F,A]) = e.insertPrev(f)
     }
         
   def async[F[_]:Type,T:Type](f:Expr[F[T]], dm: Expr[AsyncMonad[F]])(given QuoteContext):CpsChunkBuilder[F,T] =
     new CpsChunkBuilder[F,T](dm) {
        override def create() = fromFExpr(f)
        override def append[A:Type](e: CpsChunk[F,A]) = flatMapIgnore(e.toExpr)
     }


case class CpsExprResult[F[_],T](
                origin:Expr[T],
                cpsBuild: CpsChunkBuilder[F,T],
                originType: Type[T],
                haveAwait:Boolean
) {

    type MT[_] = F
    type TT = T

    def transformed(given QuoteContext): Expr[F[T]] = cpsBuild.create().toExpr
}



case class MacroError(msg: String, posExpr: Expr[_]) extends RuntimeException(msg)


erased def await[F[_],T](f:F[T]):T = ???


object Async {


  inline def transform[F[_], T](expr: =>T): F[T] =
    ${ Async.transformImpl[F,T]('expr) } 

  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(given qctx: QuoteContext): Expr[F[T]] = 
    import qctx.tasty.{_,given}
    try
      summonExpr[AsyncMonad[F]] match 
        case Some(dm) => 
             val r = rootTransform[F,T](f,dm,false).transformed
             r
        case None => 
             val ft = summon[quoted.Type[F]]
             throw MacroError(s"Can't find async monad for ${ft.show}", f)
    catch
      case ex: MacroError =>
           qctx.error(ex.msg, ex.posExpr)
           '{???}


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]], inBlock: Boolean)(
                                           given qctx: QuoteContext): CpsExprResult[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     import util._
     f match 
         case Const(c) =>   
                        val cnBuild = CpsChunkBuilder.sync(f,dm)
                        CpsExprResult(f, cnBuild, tType, false)
         case '{ _root_.cps.await[F,$fType]($ft) } => 
                        val awBuild = new CpsChunkBuilder(dm) {
                           override def create() = fromFExpr(ft)
                           override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                               flatMapIgnore(e.toExpr)
                        }
                        val awBuildCasted = awBuild.asInstanceOf[CpsChunkBuilder[F,T]]
                        CpsExprResult[F,T](f, awBuildCasted, tType, true)
         case '{ while ($cond) { $repeat }  } =>
                        val cpsCond = Async.rootTransform(cond, dm, false)
                        val cpsRepeat = Async.rootTransform(repeat, dm, false)
                        val isAsync = cpsCond.haveAwait || cpsRepeat.haveAwait
                        val builder = new CpsChunkBuilder[F,T](dm) {
                          val createExpr = '{
                             def _whilefun(): F[T] = {
                               ${cpsCond.cpsBuild.flatMap[T]( '{ c =>
                                 if (c) {
                                   ${cpsRepeat.cpsBuild.flatMapIgnore(
                                       '{ _whilefun() }
                                  ).toExpr}
                                 } else {
                                  ${pure('{()} ).toExpr.asInstanceOf[Expr[F[T]]]}
                                 }
                               }).toExpr
                               }
                             }
                             _whilefun()
                          }
                            override def create() = fromFExpr(createExpr)
                            override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                               flatMapIgnore(e.toExpr)
                        }
                        CpsExprResult[F,T](f, builder, tType, true)
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   val rFun = rootTransform(fun.seal, dm, false)
                   val builder = CpsChunkBuilder.sync(f,dm)
                   CpsExprResult(f,builder,tType,rFun.haveAwait)
                case Block(prevs,last) =>
                   val rPrevs = prevs.map{
                     case d: Definition =>
                       ???
                     case t: Term =>
                       t.seal match
                          case '{ $p:$tp } =>
                             Async.rootTransform(p,dm,true)
                          case other =>
                             ???
                   }
                   val rLast = Async.rootTransform[F,T](last.seal.asInstanceOf[Expr[T]],dm,true)
                   val lastChunk = rLast.cpsBuild.create()
                   val blockResult = rPrevs.foldRight(lastChunk)((e,s) => e.cpsBuild.append(s))
                   val haveAwait = rLast.haveAwait || rPrevs.exists(_.haveAwait)
                   val cpsBuild = new CpsChunkBuilder[F,T](dm) {
                       override def create() = CpsChunk(Seq(),blockResult.toExpr)
                       override def append[A:quoted.Type](e: CpsChunk[F,A]) =
                          if (!haveAwait)
                            e.insertPrev(f)
                          else
                            flatMapIgnore(e.toExpr)
                   }
                   CpsExprResult[F,T](f,cpsBuild,tType,haveAwait)
                case Ident(name) =>
                   val cnBuild = CpsChunkBuilder.sync(f,dm)
                   CpsExprResult(f, cnBuild , tType, false)
                case _ =>
                   printf("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
             }
     


}
