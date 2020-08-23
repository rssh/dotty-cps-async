/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020
 */
package cps

import scala.language.implicitConversions

import scala.annotation._
import scala.quoted._
import scala.compiletime._

import cps.forest._
import cps.misc._


@compileTimeOnly("await should be inside async block")
def await[F[_],T](f:F[T])(using am:CpsMonad[F]):T = ???


inline def async[F[_]](using inline am: CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       inline def apply[T](inline expr: T):F[T] =
            transform[F,T](expr)(using am)

  }

  inline def async[F[_]](using am:CpsMonad[F]): InferAsyncArg[F] =
          new InferAsyncArg[F]

  inline def transform[F[_], T](inline expr: T)(using inline m: CpsMonad[F]): F[T] =
    ${
        Async.transformImpl[F,T]('expr)
     }

  /**
   * transform expression and get monad from context.
   **/
  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(using qctx: QuoteContext): Expr[F[T]] = 
    import qctx.tasty._
    Expr.summon[CpsMonad[F]] match
       case Some(dm) =>
          transformMonad[F,T](f,dm)
       case None =>
          val ft = summon[quoted.Type[F]]
          report.throwError(s"Can't find async monad for ${ft.show}", f)


  /**
   * transform expression within given monad.  Use this function is you need to force async-transform
   * from other macros
   **/
  def transformMonad[F[_]:Type,T:Type](f: Expr[T], dm: Expr[CpsMonad[F]])(using qctx: QuoteContext): Expr[F[T]] = 
    import qctx.tasty._
    import TransformationContextMarker._
    val flags = adoptFlags(f)
    try
      if (flags.printCode)
        println(s"before transformed: ${f.show}")
      if (flags.printTree)
        println(s"value: ${f.unseal}")
      if (flags.debugLevel > 5) 
        println(s"customValueDiscard=${flags.customValueDiscard}, warnValueDiscard=${flags.warnValueDiscard}")
      val r = rootTransform[F,T](f,dm,flags,TopLevel,0, None).transformed
      if (flags.printCode)
        println(s"transformed value: ${r.show}")
        if (flags.printTree)
          println(s"transformed tree: ${r.unseal}")
      r
    catch
      case ex: MacroError =>
        report.throwError(ex.msg, ex.posExpr)


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
            val customValueDiscard = Expr.summon[cps.features.CustomValueDiscardTag].isDefined
            val warnValueDiscard = Expr.summon[cps.features.WarnValueDiscardTag].isDefined
            AsyncMacroFlags(printCode,printTree,debugLevel, true, customValueDiscard, warnValueDiscard)



  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[CpsMonad[F]],
                                      flags: AsyncMacroFlags,
                                      exprMarker: TransformationContextMarker, 
                                      nesting: Int,
                                      parent: Option[TransformationContext[_,_]])(
                                           using qctx: QuoteContext): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import qctx.tasty.{_, given _}
     import util._
     val cpsCtx = TransformationContext[F,T](f,tType,dm,flags,exprMarker,nesting,parent)
     f match 
         case Const(c) =>   ConstTransform(cpsCtx)
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
                                      marker: TransformationContextMarker)(using qctx:QuoteContext):CpsExpr[F,S]=
        rootTransform(f,cpsCtx.monad,
                      cpsCtx.flags,marker,cpsCtx.nesting+1, Some(cpsCtx))


}
