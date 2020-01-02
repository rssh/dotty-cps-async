package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps.forest._

erased def await[F[_],T](f:F[T]):T = ???

inline def async[F[_]](given am:AsyncMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object Async {

  class InferAsyncArg[F[_]](given am:AsyncMonad[F]) {

       inline def apply[T](expr: =>T):F[T] =
            transform[F,T](expr)

  }

  inline def async[F[_]](given am:AsyncMonad[F]): InferAsyncArg[F] =
          new InferAsyncArg[F]

  inline def transform[F[_], T](expr: =>T): F[T] =
    ${ Async.transformImpl[F,T]('expr) } 

  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(given qctx: QuoteContext): Expr[F[T]] = 
    summonExpr[AsyncMonad[F]] match 
      case Some(dm) => 
             val r = rootTransform[F,T](f,dm).transformed
             println(s"transformed value: ${r.show}")
             r
      case None => 
             val ft = summon[Type[F]]
             val msg = s"Can't find async monad for ${ft.show}"
             qctx.error(msg)
             throw RuntimeException(msg)


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]])(
                                           given qctx: QuoteContext): CpsExprResult[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given}
     import util._
     val cpsCtx = TransformationContext[F,T](f,tType,dm)
     f match 
         case Const(c) =>   ConstTransform(cpsCtx)
         case '{ _root_.cps.await[F,$fType]($ft) } => 
                            AwaitTransform(cpsCtx, fType, ft)
         case '{ val $x:$tx = $y } => 
                            ValDefTransform.run(cpsCtx, x, tx, y)
         case '{ if ($cond)  $ifTrue  else $ifFalse } =>
                            println("If detected!")
                            println(s"cond=${cond.unseal}")
                            ???
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   ApplyTransform(cpsCtx).run(fun,args)
                case Block(prevs,last) =>
                   BlockTransform(cpsCtx).run(prevs,last)
                case Ident(name) =>
                   IdentTransform(cpsCtx).run(name)
                case _ =>
                   printf(f.show)
                   printf("fTree:"+fTree)
                   qctx.error("language construction is not supported", f)
                   throw new IllegalStateException("language construction is not supported")
             }
     

  inline def transformMeta[F[_], T](expr: =>T): F[T] = 
           ${ Async.transformMetaImpl[F,T]('expr) }
  
    
  def transformMetaImpl[F[_], T:Type](f: Expr[T])(
                                      given qctx: QuoteContext):Expr[F[T]] = 
    val dm: AsyncMetaMonad[F] = ???
    f match 
      case Const(t) => 
           dm.pure(f) 
      case _ => print(f)
        throw new IllegalStateException("language construction is not supported")



}
