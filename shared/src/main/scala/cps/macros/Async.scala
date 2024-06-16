/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021, 2022
 */
package cps.macros

import scala.language.implicitConversions
import scala.quoted.*
import scala.compiletime.*
import scala.util.control.NonFatal
import cps.*
import cps.macros.*
import cps.macros.common.*
import cps.macros.flags.UseCompilerPlugin
import cps.macros.forest.*
import cps.macros.misc.*


object Async {

  class InferAsyncArg[F[_],C<:CpsMonadContext[F]](using val am:CpsMonad.Aux[F,C]) {

       transparent inline def apply[T](inline expr: C ?=> T) = ${
         inferAsyncArgApplyImpl[F, T, C]('am, 'expr)
       }

            //
            //am.apply(transformContextLambda(expr))
            //am.apply(x =>
            //   transform[F,T,C](expr,x)
            //)

       transparent inline def in[T](mc: CpsMonadContextProvider[F])(inline expr: mc.Context ?=> T): F[T] =
            //  TODO: compile-time check instead instance-om.
            //  Promblem,  that it should be implemented at the top level
            mc.contextualize(am.asInstanceOf[CpsTryMonad.Aux[F,C]], transformContextLambda(expr))


  }

  class InferAsyncArg1[F[_], C<:CpsTryMonadContext[F]](using val am: CpsTryMonad.Aux[F,C]) {

      transparent inline def apply[T](inline expr: C ?=> T) =
            am.apply(transformContextLambda(expr))

  }

  transparent inline def async[F[_]](using am:CpsMonad[F]) =
          new InferAsyncArg(using am)

  def inferAsyncArgApplyImpl[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](am: Expr[CpsMonad.Aux[F,C]], expr: Expr[C ?=> T])(using Quotes): Expr[F[T]] = {
    import quotes.reflect._
    val usePlugin = Expr.summon[UseCompilerPlugin.type].isDefined // ||CompilationInfo.XmacroSettings.find(_ == "cps:plugin").isDefined
    // Problem: XmacroSettings still experimental
    if (usePlugin) {
      val retval = Apply(
        TypeApply(
          Ref(Symbol.requiredMethod("cps.plugin.cpsAsyncApply")),
          List(Inferred(TypeRepr.of[F]), Inferred(TypeRepr.of[T]), Inferred(TypeRepr.of[C]))
        ),
        List(am.asTerm, expr.asTerm)
      ).asExprOf[F[T]]
      TransformUtil.findDefinitionWithoutSymbol(retval.asTerm) match
        case Some(tree) =>
          println(s"!! inferAsyncArgApplyImpl:found definition without symbol ${tree.show}")
        case None =>
          // do nothing
      val owners = TransformUtil.findAllOwnersIn(retval.asTerm)
      if (owners.size > 1) then
        println(s"!! inferAsyncArgApplyImpl: more than one owner: ${owners.mkString("\n")}")
      val incorrectDef = TransformUtil.findSubtermWithIncorrectOwner(Symbol.spliceOwner, retval.asTerm)
      if (incorrectDef.isDefined) then
        println(s"!! inferAsyncArgApplyImpl: incorrect owner: ${incorrectDef.get.show}")
      retval
    } else {
      val fun = transformContextLambdaImpl(expr)
      '{  ${am}.apply($fun) }
    }

  }

  transparent inline def transformContextLambda[F[_],T,C<:CpsMonadContext[F]](inline expr: C ?=> T): C => F[T] =
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
          report.errorAndAbort(msg, f)

             
  /**
   * transform expression within given monad.  Use this function is you need to force async-transform
   * from other macros.
   **/
  def transformMonad[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](f: Expr[T], dm: Expr[CpsMonad[F]], mc:Expr[C])(using Quotes): Expr[F[T]] =
    import quotes.reflect._
    val flags = adoptFlags(f, dm)
    val DEBUG = flags.debugLevel > 0
    try
      if flags.printCode then
        try
           println(s"before transformed: ${f.show}")
        catch
           case ex: Exception =>
              println(s"before transformed: show failed for $f, use printTree to show plain tree")
      if (flags.printTree)
        println(s"value: ${f.asTerm}")
      val r = WithOptExprProxy("cpsMonad", dm){
           dm =>
              val optRuntimeAwait = Expr.summon[CpsRuntimeAwait[F]]
              if ( flags.useLoomAwait && optRuntimeAwait.isDefined && 
                                         optRuntimeAwait.forall(_.asTerm.tpe <:< TypeRepr.of[CpsRuntimeAsyncAwait[F]])  ) {
               val cpsRuntimeAwait = optRuntimeAwait.get
               if (dm.asTerm.tpe <:< TypeRepr.of[CpsAsyncMonad[F]]) then
                  val dma = dm.asExprOf[CpsAsyncMonad[F]]
                  val transformed = loomTransform[F,T,C & CpsTryMonadContext[F]](f,dma,mc.asExprOf[C & CpsTryMonadContext[F]],cpsRuntimeAwait, flags)
                  if (dm.asTerm.tpe <:< TypeRepr.of[CpsAsyncEffectMonad[F]]) then
                     '{ ${dm.asExprOf[CpsEffectMonad[F]]}.delay(${transformed}) }
                  else if (dm.asTerm.tpe <:< TypeRepr.of[CpsSchedulingMonad[F]]) then
                     '{ ${dm.asExprOf[CpsSchedulingMonad[F]]}.spawnSync(${transformed}) }
                  else  
                     '{  ${dm}.wrap(${transformed}) }
               else
                  report.errorAndAbort(s"loom enbled but monad  ${dm.show} of type ${dm.asTerm.tpe.widen.show} is not Async, runtimeAwait = ${cpsRuntimeAwait.show}")
              } else {
               val optRuntimeAwaitProvider = Expr.summon[CpsRuntimeAwaitProvider[F]]
               val cpsExpr = rootTransform[F,T,C](f,dm,mc, optRuntimeAwait, optRuntimeAwaitProvider, flags, 0, None)
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
        report.errorAndAbort(ex.msg, ex.posExpr)



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
      val useLoomAwait = Expr.summon[cps.CpsRuntimeAwaitProvider[F]].isDefined ||
                         Expr.summon[cps.CpsRuntimeAwait[F]].isDefined
       // Expr.summon[UseLoomAwait.type].isDefined // || CompilationInfo.XmacroSettings.contains("cps:loom") - experimental
      //val pos = Position.ofMacroExpansion
      //println(s"!!!adoptFlags, useLoomAwait = ${useLoomAwait} for ${pos.sourceFile.path}:${pos.startLine}")
      AsyncMacroFlags(printCode,printTree,debugLevel, true, useLoomAwait = useLoomAwait)




  def rootTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](f: Expr[T], dm:Expr[CpsMonad[F]], mc:Expr[C], 
                                      optRuntimeAwait: Option[Expr[CpsRuntimeAwait[F]]],
                                      optRuntimeAwaitProvider: Option[Expr[CpsRuntimeAwaitProvider[F]]],
                                      flags: AsyncMacroFlags,
                                      nesting: Int,
                                      parent: Option[TransformationContext[?,?,?]])(
                                           using Quotes): CpsExpr[F,T] =
     val tType = summon[Type[T]]
     import quotes.reflect._    
     val cpsCtx = TransformationContext[F,T,C](f,tType, dm, mc,
                                               optRuntimeAwait,
                                               optRuntimeAwaitProvider,
                                               flags, nesting, parent)
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
                case afTree@Apply(fun,args) =>
                   ApplyTransform(cpsCtx).run(afTree,fun,args)
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
                case _ =>
                   println("f:"+f.show)
                   println("fTree:"+fTree)
                   throw MacroError(s"language construction is not supported: ${fTree}", f)
             }
     retval


  def nestTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type,S:Type](f:Expr[S],
                              cpsCtx: TransformationContext[F,T,C]
                              )(using Quotes):CpsExpr[F,S]=
        rootTransform(f,cpsCtx.monad, cpsCtx.monadContext,
                      cpsCtx.runtimeAwait,
                      cpsCtx.runtimeAwaitProvider,
                      cpsCtx.flags,
                      cpsCtx.nesting+1, Some(cpsCtx))


  def transformContextLambdaImpl[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](cexpr: Expr[C ?=> T])(using Quotes): Expr[C => F[T]] = {
    import quotes.reflect._

    def inInlined(t: Term, f: Term => Term): Term =
      t match
        case Inlined(call, bindings, body) => Inlined(call, bindings, f(body))
        case other => other

    def extractLambda(f: Term): (List[ValDef], Term, Term => Term) =
      f match
        case Inlined(call, bindings, body) =>
          val inner = extractLambda(body)
          (inner._1, inner._2, t => Inlined(call, bindings, t))
        case Lambda(params, body) =>
          params match
            case List(vd) => (params, body, identity)
            case _ => report.errorAndAbort(s"lambda with one argument expected, we have ${params}", cexpr)
        case Block(Nil, nested@Lambda(params, body)) => extractLambda(nested)
        case _ =>
          report.errorAndAbort(s"lambda expected, have: ${f}", cexpr)

    def transformNotInlined(t: Term): Term =
      val (oldParams, body, nestFun) = extractLambda(t)
      val oldValDef = oldParams.head
      val transformed = transformImpl[F, T, C](body.changeOwner(Symbol.spliceOwner).asExprOf[T], Ref(oldValDef.symbol).asExprOf[C])
      val mt = MethodType(List(oldValDef.name))(_ => List(oldValDef.tpt.tpe), _ => TypeRepr.of[F[T]])
      val nLambda = Lambda(Symbol.spliceOwner, mt, (owner, params) => {
        TransformUtil.substituteLambdaParams(oldParams, params, transformed.asTerm, owner).changeOwner(owner)
      })
      nestFun(nLambda)

    val retval = inInlined(cexpr.asTerm, transformNotInlined).asExprOf[C => F[T]]
    retval
  }


  //TODO: find usage and remove if not used
  def transformContextInstanceMonad[F[_]:Type,T:Type,C<:CpsTryMonadInstanceContext[F] :Type](f: Expr[T], dm: Expr[C])(using Quotes): Expr[F[T]] =
         '{
            $dm.apply( mc => ${transformMonad(f, dm, 'mc)})
          }

  def loomTransform[F[_]:Type, T:Type, C<:CpsTryMonadContext[F]:Type](f: Expr[T],
                                                        dm: Expr[CpsAsyncMonad[F]], 
                                                        ctx: Expr[C], 
                                                        runtimeApi: Expr[CpsRuntimeAwait[F]],
                                                        flags: AsyncMacroFlags,
                                                        )(using Quotes):Expr[T] = {
      //println(s"loomTransform for ${f.show}")                                                   
      loom.LoomTransform.run(f,dm,ctx,runtimeApi,flags)
   } 


}
