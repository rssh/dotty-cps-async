package cps

import scala.annotation._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime._

import cps.forest._
import cps.misc._

// erased disabled in current version
// erased def await[F[_],T](f:F[T]):T = ???

@compileTimeOnly("await should be inside async block")
def await[F[_],T](f:F[T]):T = ???

inline def async[F[_]](using am:CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       inline def apply[T](inline expr: T):F[T] =
            transform[F,T](expr)

  }

  inline def async[F[_]](using am:CpsMonad[F]): InferAsyncArg[F] =
          new InferAsyncArg[F]

  inline def transform[F[_], T](inline expr: T): F[T] =
    ${ 
        Async.transformImpl[F,T]('expr)
     } 

  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(using qctx: QuoteContext): Expr[F[T]] = 
    import qctx.tasty.{_,given _}
    val flags = adoptFlags(f)
    try
      Expr.summon[CpsMonad[F]] match 
        case Some(dm) => 
             if (flags.printCode)
                println(s"before transformed: ${f.show}")
             if (flags.printTree)
                println(s"value: ${f.unseal}")
             val r = rootTransform[F,T](f,dm,flags,"",0).transformed
             if (flags.printCode)
                println(s"transformed value: ${r.show}")
             if (flags.printTree)
                println(s"transformed tree: ${r.unseal}")
             r
        case None => 
             val ft = summon[quoted.Type[F]]
             throw MacroError(s"Can't find async monad for ${ft.show}", f)
    catch
      case ex: MacroError =>
           qctx.error(ex.msg, ex.posExpr)
           '{???}


  def adoptFlags(f: Expr[_])(using qctx: QuoteContext): AsyncMacroFlags = 
    import qctx.tasty.{_,given _}
    Expr.summon[AsyncMacroFlags] match
      case Some(flagsExpr) =>
        flagsExpr match
          case Unlifted(flags) => flags
          case _  => 
            throw MacroError(
                    s"AsyncMacroFlags ($flagsExpr) is not a compile-time value", flagsExpr )
      case None => 
            import cps.macroFlags.{_, given _}
            val printTree = Expr.summon[PrintTree.type].isDefined
            val printCode = Expr.summon[PrintCode.type].isDefined
            val debugLevel = Expr.summon[DebugLevel] match
                 case Some(expr) =>
                   expr match
                      case Unlifted(v) => v.value
                      case other  => 
                          throw MacroError(s"DebugLevel ${other.show} is not a compile-time value", other)
                 case None => 0
            AsyncMacroFlags(printCode,printTree,debugLevel)
  


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[CpsMonad[F]], 
                                      flags: AsyncMacroFlags,
                                      exprMarker: String, nesting: Int)(
                                           using qctx: QuoteContext): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given _}
     import util._
     val cpsCtx = TransformationContext[F,T](f,tType,dm,flags,exprMarker,nesting)
     f match 
         case Const(c) =>   ConstTransform(cpsCtx)
         case '{ _root_.cps.await[F,$sType]($fs) } => 
                            AwaitTransform(cpsCtx, sType, fs)
         // looks like matching error in dotty.
         // case '{ _root_.cps.await[$mType,$sType]($mst) } => 
         //                    AwaitTransformOtherMonad(cpsCtx, mType, sType, mst.asInstanceOf[Expr[Any]])
         case '{ if ($cond)  $ifTrue  else $ifFalse } =>
                            IfTransform.run(cpsCtx, cond, ifTrue, ifFalse)
         case '{ while ($cond) { $repeat }  } =>
                            WhileTransform.run(cpsCtx, cond, repeat)
         //case '{ try $body catch $cases finally $finalizer   } =>
         //                  can't be determinated inside matching
         case '{ throw $ex } =>
                            ThrowTransform.run(cpsCtx, ex)
         case _ => 
             val fTree = f.unseal
             fTree match {
                case Apply(fun,args) =>
                   ApplyTransform(cpsCtx).run(fun,args)
                case TypeApply(fun,args) =>
                   TypeApplyTransform(cpsCtx).run(fun,args)
                case Assign(left,right) =>
                   AssignTransform(cpsCtx).run(left,right)
                case lambda@Lambda(params, body) =>
                   LambdaTreeTransform.run(cpsCtx,lambda,params,body)
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
                case thisTerm@This(qual) =>
                   ThisTransform(cpsCtx).run(thisTerm)
                case matchTerm@Match(scrutinee, caseDefs) =>
                   MatchTreeTransform.run(cpsCtx, matchTerm)
                case selectTerm: Select =>
                   SelectTreeTransform.run(cpsCtx, selectTerm)
                //   SelectOuter ? //TreeTransform.run(cpsCtx, selectTerm)
                case inlinedTerm@ Inlined(call,bindings,body) =>
                   InlinedTransform(cpsCtx).run(inlinedTerm)
                case superTerm@Super(qual,mix) =>
                   SuperTransform(cpsCtx).run(superTerm)
                case returnTerm@Return(expr)=>
                   ReturnTransform(cpsCtx).run(returnTerm)
                case _ =>
                   printf("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
             }
     
   
  def nestTransform[F[_]:Type,T:Type,S:Type](f:Expr[S], 
                                             cpsCtx: TransformationContext[F,T], 
                                             marker:String)(using qctx:QuoteContext):CpsExpr[F,S]=
        rootTransform(f,cpsCtx.monad,
                      cpsCtx.flags,cpsCtx.exprMarker+marker,cpsCtx.nesting+1)


}
