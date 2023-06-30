package cps.stream

import scala.util.*
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.*
import scala.quoted.*
import scala.concurrent.*
import cps.{*, given}
import cps.macros.common.*
import cps.macros.flags.UseCompilerPlugin
import cps.macros.misc.*
import cps.plugin.*


/**
 * This typeclass should be implemented for streams, which can be 
 * a target of asyncStream.
 * i.e. if we want use generator of form
 * ```
 *  asyncStream[R] { out =>
 *     ...
 *     out.emit(value)
 *  }
 * ```
 * Then we should have an implementation of given `CpsAsyncEmitAbsorber[R]` in the current scope.
 **/
trait CpsAsyncEmitAbsorber[R]:

   type Monad[_]
   type Element
   type Context <:CpsMonadContext[Monad]

   def eval(f: Context => CpsAsyncEmitter[Monad,Element] => Monad[Unit]): R

   def asyncMonad: CpsAsyncMonad.Aux[Monad,Context]


object CpsAsyncEmitAbsorber:

   type Aux[R, F[_], C<:CpsMonadContext[F], T] = CpsAsyncEmitAbsorber[R]  {

      type Monad[X] = F[X]
      type Context = C
      type Element = T
       
   }


trait CpsAsyncEmitAbsorber4[R, F[_], C<:CpsMonadContext[F], T](using val auxAsyncMonad: CpsAsyncMonad.Aux[F,C]) extends CpsAsyncEmitAbsorber[R]:

   override type Element = T
   override type Monad[X] = F[X]
   override type Context = C


//TODO: fill bug report for dotty, about error in -Xcheck-macros when a is not val
class AsyncStreamHelper[R,F[_],C<:CpsMonadContext[F],A](val a: CpsAsyncEmitAbsorber.Aux[R,F,C,A]){

    transparent inline def apply(inline f: C ?=> CpsAsyncEmitter[F,A] => Unit): R = ${
         CpsAsyncStreamMacro.transform[R,F,C,A]('f, 'a)
    }
       

}

object CpsAsyncStreamMacro:

    def transform[R:Type, F[_]:Type, C<:CpsMonadContext[F]:Type, T:Type](f: Expr[ C ?=> CpsAsyncEmitter[F,T] => Unit], 
                                             absorber: Expr[CpsAsyncEmitAbsorber.Aux[R,F,C,T]])(using Quotes): Expr[R] = {
          import quotes.reflect._
          Expr.summon[UseCompilerPlugin] match
            case Some(_) =>
              val refCpsAsyncStreamAppky = Ref(Symbol.requiredMethod("cps.plugin.cpsAsyncStreamApply"))
              Apply(
                TypeApply( refCpsAsyncStreamAppky, List(TypeTree.of[R], TypeTree.of[F], TypeTree.of[T], TypeTree.of[C])),
                List(absorber.asTerm, f.asTerm)
              ).asExprOf[R]
            case None =>
              val r = transformTree[R,F,C,T](f.asTerm, absorber)
              r.asExprOf[R]
    }
          
    def transformTree[R:Type, F[_]:Type, C<:CpsMonadContext[F]:Type, T:Type](using qctx: Quotes)(
                           f:qctx.reflect.Term, 
                           absorber: Expr[CpsAsyncEmitAbsorber.Aux[R,F,C,T]] ) : qctx.reflect.Term = {
         import quotes.reflect.*
         insideLambda(f,
            (context, fBody) =>
               insideLambda(fBody, { (param, body) =>
                  val mt1 = MethodType(List(context.name))(_ => List(context.tpt.tpe), 
                                                                _ => TypeRepr.of[CpsAsyncEmitter[F,T] => F[Unit]])
                  val lambda1 = Lambda(Symbol.spliceOwner, mt1, (owner, contextArgs) =>
                     val mt2 = MethodType(List(param.name))(_ => List(param.tpt.tpe),
                                                                _ => TypeRepr.of[F[Unit]])
                     val contextTerm = contextArgs.head.asInstanceOf[Term]
                     val lambda2 = Lambda(owner, mt2, {(owner, args) =>
                        val emitterTerm = args.head.asInstanceOf[Term]
                        val nBody = cps.macros.common.TransformUtil.substituteLambdaParams(
                                          List(param,context), List(args.head, contextArgs.head), body, owner)
                        // TODO: move to cpsAsync
                        val asyncBody = cps.macros.Async.transformMonad[F,Unit,C](nBody.asExprOf[Unit],
                                                                                 '{ ${absorber}.asyncMonad },
                                                                                 contextTerm.asExprOf[C] 
                                                                                 ).asTerm
                        asyncBody.changeOwner(owner)
                     })
                     Block(Nil,lambda2).changeOwner(owner)
                  )
                  val nLambda = Block(Nil,lambda1).changeOwner(Symbol.spliceOwner)
                  '{ 
                       ${absorber}.eval(${nLambda.asExprOf[C => CpsAsyncEmitter[F,T] => F[Unit]]})
                   }.asTerm
               })
         )     
    }


    def insideLambda(using qctx: Quotes)(f:qctx.reflect.Term, 
            op: (qctx.reflect.ValDef, qctx.reflect.Term) => qctx.reflect.Term) : qctx.reflect.Term =
          import quotes.reflect.*
          f match
            case Inlined(name,List(),body) => insideLambda(body, op)
            case Inlined(name,bindings,body) => Inlined(name,bindings,insideLambda(body, op))
            case Lambda(params,body) =>
               if ( !params.headOption.isDefined ||  params.tail.headOption.isDefined) then {
                   throw MacroError("expected, that lambda in transform have one argument",f.asExpr);
               }
               val param = params.head
               op(param, body)
            case Block(List(), last) => insideLambda(last, op)
            case Block(stats, last) => Block(stats, insideLambda(last, op))
            case _ =>
                throw MacroError("lambda expected in asyncStream", f.asExpr)
