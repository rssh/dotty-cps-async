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

  class InferAsyncArg[F[_],C<:CpsMonadContext[F]](using val am:CpsMonad.Aux[F,C]) {

       transparent inline def apply[T](inline expr: C ?=> T) =
            //transform[F,T](using am)(expr)
            am.apply(transformContextLambda(expr))
            //am.apply(x =>
            //   transform[F,T,C](expr,x)
            //)

       
       transparent inline def in[T](using mc: CpsMonadContextProvider[F] )(inline expr: mc.Context ?=> T ): F[T]  = 
            mc.contextualize(transformContextLambda(expr))
       
  }

  inline def async[F[_]](using am:CpsMonad[F]) =
          new InferAsyncArg(using am)
     

  transparent inline def transformContextLambda[F[_],T,C<:CpsMonadContext[F]](inline expr: C ?=> T)(using m: CpsMonad[F]): C => F[T] =
     ${
        Async.transformContextLambdaImpl[F,T,C]('expr)
     } 

  transparent inline def transform[F[_],T,C<:CpsMonadContext[F]](inline expr: T, inline ctx: C)(using m: CpsMonad[F]): F[T] =
     ${
        Async.transformImpl[F,T,C]('expr, 'ctx)
     } 
 
  /**
   * transform expression and get monad from context.
   *@param f - expression to transform
   *@param c - monad context parameter
   **/
  def transformImpl[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](f: Expr[T], c:Expr[C])(using Quotes): Expr[F[T]] =
    import quotes.reflect._
    Expr.summon[CpsMonad[F]] match
       case Some(dm) =>
          transformMonad[F,T,C](f,dm,c)
       case None =>
          val msg = s"Can't find async monad for ${TypeRepr.of[F].show} (transformImpl)"
          report.throwError(msg, f)

             
  /**
   * transform expression within given monad.  Use this function is you need to force async-transform
   * from other macros.
   **/
  def transformMonad[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](f: Expr[T], dm: Expr[CpsMonad[F]], mc:Expr[C])(using Quotes): Expr[F[T]] =
    import quotes.reflect._
    val flags = adoptFlags(f, dm)
    val DEBUG = true
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
          if (resolvedMemoization.kind !=  CpsMonadMemoization.Kind.BY_DEFAULT) then
             observatory.effectColoring.enabled = true
          Some(resolveMemoization[F,T](f,dm))
        else None
      observatory.analyzeTree[F]
      val r = WithOptExprProxy("cpsMonad", dm){
           dm => 
              val cpsExpr = rootTransform[F,T,C](f,dm,mc,memoization,flags,observatory,0, None)
              if (DEBUG) {
                 TransformUtil.dummyMapper(cpsExpr.transformed.asTerm, Symbol.spliceOwner)
              }
              val retval = if (dm.asTerm.tpe <:< TypeRepr.of[CpsEffectMonad[F]]) then       
                 '{ ${dm.asExprOf[CpsEffectMonad[F]]}.flatDelay(${cpsExpr.transformed}) }
              else if (dm.asTerm.tpe <:< TypeRepr.of[CpsSchedulingMonad[F]]) then       
                 '{ ${dm.asExprOf[CpsSchedulingMonad[F]]}.spawn(${cpsExpr.transformed}) }
              else
                 if (flags.debugLevel > 10) then
                    println(s"dm.asTerm.tpe = ${dm.asTerm.tpe.show} not implements CpsEffectMonad[F], f=$Type[F].show")
                 cpsExpr.transformed
              if (DEBUG) {
                 try {
                     TransformUtil.dummyMapper(retval.asTerm, Symbol.spliceOwner)
                 }catch{
                    case ex: Throwable =>
                     println(s"failed term: ${retval.show}")
                     throw ex
                 }      
              } 
              retval
      }
      if (DEBUG) {
         TransformUtil.dummyMapper(r.asTerm, Symbol.spliceOwner)
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
            val customValueDiscard = Expr.summon[cps.ValueDiscard.CustomTag].isDefined || automaticColoring
            val warnValueDiscard = Expr.summon[cps.ValueDiscard.WarnTag].isDefined || 
                                     (automaticColoring && 
                                      Expr.summon[cps.automaticColoring.WarnValueDiscard[F]].isDefined )
            AsyncMacroFlags(printCode,printTree,debugLevel, true, customValueDiscard, warnValueDiscard, automaticColoring)


  def resolveMemoization[F[_]:Type, T:Type](f: Expr[T], dm: Expr[CpsMonad[F]])(using Quotes): 
                                                                  TransformationContext.Memoization[F] =
     import cps.monads._
     import quotes.reflect._
     Expr.summon[CpsMonadMemoization[F]] match
       case Some(mm) =>
             val mmtp = mm.asTerm.tpe
             if (mmtp <:< TypeRepr.of[CpsMonadMemoization.Default[F]]) then
                TransformationContext.Memoization[F](CpsMonadMemoization.Kind.BY_DEFAULT, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadMemoization.Inplace[F]]) then
                TransformationContext.Memoization[F](CpsMonadMemoization.Kind.INPLACE, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadMemoization.Pure[F]]) then
                TransformationContext.Memoization[F](CpsMonadMemoization.Kind.PURE, mm )
             else if (mmtp <:< TypeRepr.of[CpsMonadMemoization.Dynamic[F]]) then
                TransformationContext.Memoization[F](CpsMonadMemoization.Kind.DYNAMIC, mm )
             else
                throw MacroError(s"Can't extract memoization kind from ${mm.show} for ${TypeRepr.of[F].show}", mm)
       case None =>
             throw MacroError(s"Can't resolve CpsMonadMemoization for ${TypeRepr.of[F].show}", f)
             
                


  def rootTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](f: Expr[T], dm:Expr[CpsMonad[F]], mc:Expr[C], 
                                      optMemoization: Option[TransformationContext.Memoization[F]],
                                      flags: AsyncMacroFlags,
                                      observatory: Observatory.Scope#Observatory,
                                      nesting: Int,
                                      parent: Option[TransformationContext[_,_,_]])(
                                           using Quotes): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import quotes.reflect._    
     val cpsCtx = TransformationContext[F,T,C](f,tType,dm,mc,optMemoization,flags,observatory,nesting,parent)
     val retval = f match 
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
     if (true) then // check
         val dummyMap = new TreeMap() {}
         try {
            val x = dummyMap.transformTerm(retval.transformed.asTerm)(Symbol.spliceOwner) 
         }catch{
            case ex: Throwable =>
               println(s"Here, input term is ${f.show}, tree:${f.asTerm}")
               throw ex;
         }  
     retval


  def nestTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type,S:Type](f:Expr[S],
                              cpsCtx: TransformationContext[F,T,C]
                              )(using Quotes):CpsExpr[F,S]=
        rootTransform(f,cpsCtx.monad, cpsCtx.monadContext, cpsCtx.memoization,
                      cpsCtx.flags,cpsCtx.observatory,cpsCtx.nesting+1, Some(cpsCtx))


  def transformContextLambdaImpl[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](cexpr: Expr[C ?=> T])(using Quotes): Expr[C => F[T]] =
      import quotes.reflect._

      def inInlined(t: Term, f: Term => Term): Term =
         t match
            case Inlined(call, bindings, body) => Inlined(call, bindings, f(body))
            case other => other

      def extractLambda(f:Term): (List[ValDef], Term, Term => Term ) =
         f match
            case Inlined(call, bindings, body) =>
               val inner = extractLambda(body)
               (inner._1, inner._2, t => Inlined(call, bindings, t) )   
            case Lambda(params,body) =>
               params match
                  case List(vd) => (params, body, identity)
                  case _ => report.throwError(s"lambda with one argument expected, we have ${params}",cexpr)
            case Block(Nil,nested@Lambda(params,body)) => extractLambda(nested)
            case _ =>
               report.throwError(s"lambda expected, have: ${f}", cexpr)
      
      def transformNotInlined(t: Term): Term =
         val (oldParams, body, nestFun) = extractLambda(t)
         val oldValDef = oldParams.head
         val transformed = transformImpl[F,T,C](body.asExprOf[T], Ref(oldValDef.symbol).asExprOf[C])
         val mt = MethodType(List(oldValDef.name))( _ => List(oldValDef.tpt.tpe), _ => TypeRepr.of[F[T]])
         val nLambda = Lambda(Symbol.spliceOwner, mt, (owner, params) => {
            TransformUtil.substituteLambdaParams( oldParams, params, transformed.asTerm, owner ).changeOwner(owner)
         })
         nestFun(nLambda)

      val retval = inInlined(cexpr.asTerm, transformNotInlined).asExprOf[C => F[T]]
      retval


  def transformContextInstanceMonad[F[_]:Type,T:Type,C<:CpsMonadInstanceContext[F] :Type](f: Expr[T], dm: Expr[C])(using Quotes): Expr[F[T]] = 
         '{
            $dm.apply( mc => ${transformMonad(f, dm, 'mc)})
          }

}
