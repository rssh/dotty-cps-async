package cps

import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps.forest._
import cps.misc._

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
    import qctx.tasty.{_,given}
    try
      summonExpr[AsyncMonad[F]] match 
        case Some(dm) => 
             println(s"before transformed: ${f.show}")
             println(s"value: ${f.unseal}")
             val r = rootTransform[F,T](f,dm,false).transformed
             println(s"transformed value: ${r.show}")
             println(s"transformed tree: ${r.unseal}")
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
     val cpsCtx = TransformationContext[F,T](f,tType,dm, inBlock)
     f match 
         case Const(c) =>   ConstTransform(cpsCtx)
         case '{ _root_.cps.await[F,$sType]($fs) } => 
                            AwaitTransform(cpsCtx, sType, fs)
        // looks like matching error in dotty.
        // case '{ _root_.cps.await[$mType,$sType]($mst) } => 
        //                    AwaitTransformOtherMonad(cpsCtx, mType, sType, mst.asInstanceOf[Expr[Any]])
         case '{ val $x:$tx = $y } if inBlock => 
                            ValDefTransform.run(cpsCtx, x, tx, y)
         case '{ var $x:$tx = $y } if inBlock => 
                            ValDefTransform.run(cpsCtx, x, tx, y)
         case '{ if ($cond)  $ifTrue  else $ifFalse } =>
                            IfTransform.run(cpsCtx, cond, ifTrue, ifFalse)
         case '{ while ($cond) { $repeat }  } =>
                            WhileTransform.run(cpsCtx, cond, repeat)
         //case '{ try $body catch $cases finally $finalizer   } =>
         //                  can't be determinated inside matching
         case '{ throw $ex } =>
                            ThrowTransform.run(cpsCtx, ex)
         case _ => 
             val fTree = f.unseal.underlyingArgument
             fTree match {
                case Apply(fun,args) =>
                   ApplyTransform(cpsCtx).run(fun,args)
                case Assign(left,right) =>
                   print(s"Assign detected, left=${left}, right=${right}")
                   AssignTransform(cpsCtx).run(left,right)
                case Lambda(params, body) =>
                   print(s"Lambda detected, params=${params}, body=${body}")
                   ???
                case Block(prevs,last) =>
                   BlockTransform(cpsCtx).run(prevs,last)
                case Ident(name) =>
                   IdentTransform(cpsCtx).run(name)
                case Typed(expr, tp) =>
                   TypedTransform(cpsCtx).run(expr,tp)
                case Try(body, cases, finalizer) =>
                   TryTransform(cpsCtx).run(body,cases,finalizer)
                case New(typeTree) =>
                   NewTransform(cpsCtx).run(typeTree)
                case selectTerm: Select =>
                   SelectTreeTransform.run(cpsCtx, selectTerm)
                case _ =>
                   printf("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
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
