/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021
 */
package cps.macros

import scala.language.implicitConversions

import scala.quoted._
import scala.compiletime._

import cps.*
import cps.macros.*
import cps.macros.forest.*
import cps.macros.misc.*
import cps.macros.observatory.*


object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       transparent inline def apply[T](inline expr: T) =
            transform[F,T](expr)(using am)

  }

  inline def async[F[_]](using am:CpsMonad[F]): InferAsyncArg[F] =
          new InferAsyncArg[F]

  transparent inline def transform[F[_], T](inline expr: T)(using m: CpsMonad[F]) =
    ${
        Async.transformImpl[F,T]('expr)
     }

  /**
   * transform expression and get monad from context.
   **/
  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(using Quotes): Expr[F[T]] =
    import quotes.reflect._
    Expr.summon[CpsMonad[F]] match
       case Some(dm) =>
          transformMonad[F,T](f,dm)
       case None =>
          val msg = s"Can't find async monad for ${TypeRepr.of[F].show} (transformImpl)"
          report.throwError(msg, f)


  /**
   * transform expression within given monad.  Use this function is you need to force async-transform
   * from other macros
   **/
  def transformMonad[F[_]:Type,T:Type](f: Expr[T], dm: Expr[CpsMonad[F]])(using Quotes): Expr[F[T]] =
    import quotes.reflect._
    val flags = adoptFlags(f, dm)
    try
      if flags.printCode then
        try
           println(s"before transformed: ${f.show}")
        catch
           case ex: Exception =>
              println(s"before transformed: show failed for $f, use printTree to show plain tree")
      if (flags.printTree)
        println(s"value: ${f.asTerm}")
      if (flags.debugLevel > 5) 
        println(s"customValueDiscard=${flags.customValueDiscard}, warnValueDiscard=${flags.warnValueDiscard}, automaticColoring=${flags.automaticColoring}")
      val observatory = Observatory(f.asTerm, flags)
      val memoization: Option[TransformationContext.Memoization[F]] = 
        if flags.automaticColoring then
          val resolvedMemoization = resolveMemoization[F,T](f,dm)
          if (resolvedMemoization.kind !=  MonadMemoizationKind.BY_DEFAULT) then
             observatory.effectColoring.enabled = true
          Some(resolveMemoization[F,T](f,dm))
        else None
      observatory.analyzeTree[F]
      val r = WithOptExprProxy("cpsMonad", dm){
           dm => 
              val cpsExpr = rootTransform[F,T](f,dm,memoization,flags,observatory,0, None)
              if (dm.asTerm.tpe <:< TypeRepr.of[CpsEffectMonad[F]]) then       
                 '{ ${dm.asExprOf[CpsEffectMonad[F]]}.flatDelay(${cpsExpr.transformed}) }
              else
                 if (flags.debugLevel > 10) then
                    println(s"dm.asTerm.tpe = ${dm.asTerm.tpe.show} not implements CpsEffectMonad[F], f=$Type[F].show")
                 cpsExpr.transformed
      }
      if (flags.printCode)
        try
           println(s"transformed value: ${r.show}")
        catch
           case ex: Exception =>
              println(s"after transformed: show failed for $r, use printTree to show plain tree")
      if (flags.printTree)
        println(s"transformed tree: ${r.asTerm}")
      r
    catch
      case ex: MacroError =>
        if (flags.debugLevel > 0)
           ex.printStackTrace
        report.throwError(ex.msg, ex.posExpr)


  def adoptFlags[F[_]:Type,T](f: Expr[T], dm: Expr[CpsMonad[F]])(using Quotes): AsyncMacroFlags =
    import quotes.reflect._
    /*
    Expr.summon[AsyncMacroFlags] match
      case Some(flagsExpr) =>
        flagsExpr match
          case Expr(flags) => flags
          case _  =>
            throw MacroError(
                    s"AsyncMacroFlags ($flagsExpr) is not a compile-time value", flagsExpr )
      case None =>
     */
       import cps.macros.flags.{*, given}
            val printTree = Expr.summon[PrintTree.type].isDefined
            val printCode = Expr.summon[PrintCode.type].isDefined
            val debugLevel = Expr.summon[DebugLevel] match
                 case Some(expr) =>
                   expr match
                      case Expr(v) => v.value
                      case other  =>
                          throw MacroError(s"DebugLevel ${other.show} is not a compile-time value", other)
                 case None => 0
            val automaticColoringTag = Expr.summon[cps.automaticColoring.AutomaticColoringTag[F]]
            val automaticColoring = automaticColoringTag.isDefined
            if (debugLevel > 0)
               println(s"automaticColoringTag: ${automaticColoringTag.map(_.show)}")
            val customValueDiscard = Expr.summon[cps.customValueDiscard.Tag].isDefined || automaticColoring
            val warnValueDiscard = Expr.summon[cps.warnValueDiscard.Tag].isDefined || 
                                     (automaticColoring && 
                                      Expr.summon[cps.automaticColoring.WarnValueDiscard[F]].isDefined )
            AsyncMacroFlags(printCode,printTree,debugLevel, true, customValueDiscard, warnValueDiscard, automaticColoring)


  def resolveMemoization[F[_]:Type, T:Type](f: Expr[T], dm: Expr[CpsMonad[F]])(using Quotes): 
                                                                  TransformationContext.Memoization[F] =
     import cps.automaticColoring.{given,*}
     import quotes.reflect._
     Expr.summon[CpsMonadMemoization[F]] match
       case Some(mm) =>
             val mmtp = mm.asTerm.tpe
             if (mmtp <:< TypeRepr.of[CpsMonadDefaultMemoization[F]]) then
                TransformationContext.Memoization[F](MonadMemoizationKind.BY_DEFAULT, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadInplaceMemoization[F]]) then
                TransformationContext.Memoization[F](MonadMemoizationKind.INPLACE, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadPureMemoization[F]]) then
                TransformationContext.Memoization[F](MonadMemoizationKind.PURE, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadDynamicMemoization[F]]) then
                TransformationContext.Memoization[F](MonadMemoizationKind.DYNAMIC, mm )
             else
                throw MacroError(s"Can't extract memoization kind from ${mm.show} for ${TypeRepr.of[F].show}", mm)
       case None =>
             throw MacroError(s"Can't resolve CpsMonadMemoization for ${TypeRepr.of[F].show}", f)
             
                


  def rootTransform[F[_]:Type,T:Type](f: Expr[T], dm:Expr[CpsMonad[F]], 
                                      optMemoization: Option[TransformationContext.Memoization[F]],
                                      flags: AsyncMacroFlags,
                                      observatory: Observatory.Scope#Observatory,
                                      nesting: Int,
                                      parent: Option[TransformationContext[_,_]])(
                                           using Quotes): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import quotes.reflect._
     val cpsCtx = TransformationContext[F,T](f,tType,dm,optMemoization,flags,observatory,nesting,parent)
     f match 
         case '{ if ($cond)  $ifTrue  else $ifFalse } =>
                            IfTransform.run(cpsCtx, cond, ifTrue, ifFalse)
         case '{ while ($cond) { $repeat }  } =>
                            WhileTransform.run(cpsCtx, cond, repeat)
         //case '{ try $body catch $cases finally $finalizer   } =>
         //                  can't be determinated inside matching
         case '{ throw $ex } =>
                            ThrowTransform.run(cpsCtx, ex)
         case _ =>
             val fTree = f.asTerm
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
                case tryTerm@Try(body, cases, finalizer) =>
                   TryTransform(cpsCtx).run(tryTerm,body,cases,finalizer)
                case New(typeTree) =>
                   NewTransform(cpsCtx).run(typeTree)
                case thisTerm@This(qual) =>
                   ThisTransform(cpsCtx).run(thisTerm)
                case matchTerm@Match(scrutinee, caseDefs) =>
                   MatchTreeTransform.run(cpsCtx, matchTerm)
                case selectOuterTerm: SelectOuter =>
                   SelectOuterTreeTransform.run(cpsCtx, selectOuterTerm)
                case selectTerm: Select =>
                   SelectTreeTransform.run(cpsCtx, selectTerm)
                //   SelectOuter ? //TreeTransform.run(cpsCtx, selectTerm)
                case inlinedTerm@ Inlined(call,bindings,body) =>
                   InlinedTreeTransform.run(cpsCtx,inlinedTerm)
                case superTerm@Super(qual,mix) =>
                   SuperTransform(cpsCtx).run(superTerm)
                case returnTerm@Return(expr, from)=>
                   ReturnTransform(cpsCtx).run(returnTerm, from)
                case constTerm@Literal(_)=>  
                   ConstTransform.run(cpsCtx, constTerm)
                case repeatedTerm@Repeated(elems, tpt) =>  
                   RepeatedTransform(cpsCtx).run(repeatedTerm)
                case _ =>
                   println("f:"+f.show)
                   println("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
             }


  def nestTransform[F[_]:Type,T:Type,S:Type](f:Expr[S],
                              cpsCtx: TransformationContext[F,T]
                              )(using Quotes):CpsExpr[F,S]=
        rootTransform(f,cpsCtx.monad, cpsCtx.memoization,
                      cpsCtx.flags,cpsCtx.observatory,cpsCtx.nesting+1, Some(cpsCtx))


}
