package cps

import scala.quoted._
import scala.quoted.matching._

trait CB[T] 

object CBF {
   def pure[T](value:T): CB[T] = ???
   def map[A,B](fa:CB[A])(f: A=>B):CB[B] = ???
   def flatMap[A,B](fa:CB[A])(f: A=>CB[B]):CB[B] = ???
}


case class CpsChunk[T](prev: Seq[Expr[_]], last:Expr[CB[T]]) 

    def toExpr(given QuoteContext): Expr[CB[T]] =
      if (prev.isEmpty)
        last
      else
        Expr.block(prev.toList,last)


trait CpsChunkBuilder[T:Type]

  def create(): CpsChunk[T]

  def append[A:Type](chunk: CpsChunk[A]): CpsChunk[A]
  
  protected def fromFExpr(f: Expr[CB[T]]): CpsChunk[T] =
          CpsChunk(Seq(),f)


  def flatMap[A:Type](t: Expr[T => CB[A]])(given QuoteContext): CpsChunk[A] =
                 CpsChunk[A](Seq(), 
                      '{ CBF.flatMap(${create().toExpr})(${t}) }
                 )

  def flatMapIgnore[A:Type](t: Expr[CB[A]])(given QuoteContext): CpsChunk[A] =
           CpsChunk[A](Seq(), 
                 '{ CBF.flatMap(${create().toExpr})(_ => ${t}) }
           )

object CpsChunkBuilder 

   def sync[T:Type](f:Expr[T])(given QuoteContext):CpsChunkBuilder[T] =
     new CpsChunkBuilder[T] {
        override def create() = fromFExpr('{ CBF.pure($f) })
        override def append[A:Type](e: CpsChunk[A]) = 
            CpsChunk(f +: e.prev, e.last)
     }
         
   def async[T:Type](f:Expr[CB[T]])(given QuoteContext):CpsChunkBuilder[T] =
     new CpsChunkBuilder[T] {
        override def create() = fromFExpr(f)
        override def append[A:Type](e: CpsChunk[A]) = flatMapIgnore(e.toExpr)
     }


case class CpsExprResult[T](
                cpsBuild: CpsChunkBuilder[T],
                haveAwait:Boolean
) {
    def transformed(given QuoteContext): Expr[CB[T]] = cpsBuild.create().toExpr
}


erased def await[T](f: CB[T]):T = ???


object Async {

  inline def transform[T](expr: =>T): CB[T] =
    ${ Async.transformImpl[T]('expr) } 

  def transformImpl[T:Type](f: Expr[T])(given qctx: QuoteContext): Expr[CB[T]] = 
    import qctx.tasty.{_,given}
    rootTransform[T](f).transformed

  def rootTransform[T:Type](f: Expr[T])(given qctx: QuoteContext): CpsExprResult[T] =
     import qctx.tasty.{_, given}
     import util._
     f match 
         case Const(c) =>   
                        val cnBuild = CpsChunkBuilder.sync(f)
                        CpsExprResult(cnBuild, false)
         case '{ _root_.cps.await[$fType]($ft) } => 
                        val awBuild = CpsChunkBuilder.async(ft)
                        val awBuildCasted = awBuild.asInstanceOf[CpsChunkBuilder[T]]
                        CpsExprResult[T](awBuildCasted, true)
         case '{ while ($cond) { $repeat }  } =>
                        val cpsCond = Async.rootTransform(cond)
                        val cpsRepeat = Async.rootTransform(repeat)
                        val isAsync = cpsCond.haveAwait || cpsRepeat.haveAwait
                        val builder = CpsChunkBuilder.async(
                          '{
                             def _whilefun(): CB[T] = {
                               ${cpsCond.cpsBuild.flatMap[T]( '{ c =>
                                 if (c) {
                                   ${cpsRepeat.cpsBuild.flatMapIgnore(
                                       '{ _whilefun() }
                                  ).toExpr}
                                 } else {
                                  CBF.pure(()).asInstanceOf[CB[T]]
                                 }
                               }).toExpr
                               }
                             }
                             _whilefun()
                          })
                        CpsExprResult[T](builder, true)
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   val rFun = rootTransform(fun.seal)
                   val builder = CpsChunkBuilder.sync(f)
                   CpsExprResult(builder,rFun.haveAwait)
                case Block(prevs,last) =>
                   val rPrevs = prevs.map{
                     case d: Definition =>
                       ???
                     case t: Term =>
                       t.seal match
                          case '{ $p:$tp } =>
                             Async.rootTransform(p)
                          case other =>
                             ???
                   }
                   val rLast = Async.rootTransform[T](last.seal.asInstanceOf[Expr[T]])
                   val lastChunk = rLast.cpsBuild.create()
                   val blockResult = rPrevs.foldRight(lastChunk)((e,s) => e.cpsBuild.append(s))
                   val haveAwait = rLast.haveAwait || rPrevs.exists(_.haveAwait)
                   val cpsBuild =  CpsChunkBuilder.async[T](blockResult.toExpr)
                   CpsExprResult[T](cpsBuild,haveAwait)
                case Ident(name) =>
                   val cnBuild = CpsChunkBuilder.sync(f)
                   CpsExprResult(cnBuild , false)
                case _ =>
                   printf("fTree:"+fTree)
                   ???
             }

}
