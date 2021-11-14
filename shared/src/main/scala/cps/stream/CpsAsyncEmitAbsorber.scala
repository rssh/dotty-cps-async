package cps.stream

import scala.util._
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic._

import scala.quoted._
import scala.concurrent._

import cps.{*,given}
import cps.macros.misc.*


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

   def eval(f: CpsAsyncEmitter[Monad,Element] => Monad[Unit])(using CpsContextType[Monad]): R

   def asyncMonad: CpsAsyncMonad[Monad]


object CpsAsyncEmitAbsorber:

   type Aux[R, F[_], T] = CpsAsyncEmitAbsorber[R]  {

      type Monad[X] = F[X]
      type Element = T
       
   }


trait CpsAsyncEmitAbsorber3[R, F[_]: CpsAsyncMonad, T] extends CpsAsyncEmitAbsorber[R]:

   override type Element = T
   override type Monad[X] = F[X]


class AsyncStreamHelper[R,F[_],A](a: CpsAsyncEmitAbsorber.Aux[R,F,A]){

    // TODO: add a.Context

    transparent inline def apply(inline f: CpsAsyncEmitter[F,A] => Unit): R = ${
         CpsAsyncStreamMacro.transform[R,F,A]('f, 'a)
    }
       

}

object CpsAsyncStreamMacro:

    def transform[R:Type, F[_]:Type, T:Type](f: Expr[CpsAsyncEmitter[F,T] => Unit], 
                                             absorber: Expr[CpsAsyncEmitAbsorber.Aux[R,F,T]])(using Quotes): Expr[R] =
          import quotes.reflect._
          val r = transformTree[R,F,T](f.asTerm, absorber)
          r.asExprOf[R]
          
             
    def transformTree[R:Type, F[_]:Type, T:Type](using qctx: Quotes)(f:qctx.reflect.Term, 
                                          absorber: Expr[CpsAsyncEmitAbsorber.Aux[R,F,T]] ) : qctx.reflect.Term =
          import quotes.reflect.*
          f match
            case Inlined(name,List(),body) => transformTree[R,F,T](body, absorber)
            case Inlined(name,bindings,body) => Inlined(name,bindings,transformTree[R,F,T](body, absorber))
            case Lambda(params,body) =>
               if ( !params.headOption.isDefined ||  params.tail.headOption.isDefined) then {
                   throw MacroError("expected, that lambda in transform have one argument",f.asExpr);
               }

               Expr.summon[CpsContextType[F]] match
                  case Some(ctp) =>
                     val param = params.head
                     val mt = MethodType(List(param.name))(_ => List(param.tpt.tpe), _ => TypeRepr.of[F].appliedTo(body.tpe.widen))
                     val nLambda = Lambda(Symbol.spliceOwner, mt, (symbol, args) => {
                        val argTerm = args.head.asInstanceOf[Term]

                        val nBody = cps.macros.forest.TransformUtil.substituteLambdaParams(params,args, body, symbol)
                        val asyncBody = cps.macros.Async.transformMonad[F,Unit](nBody.asExprOf[Unit], '{ ${absorber}.asyncMonad }).asTerm
                        asyncBody.changeOwner(symbol)
                     })
                     '{ 
                       ${absorber}.eval(${nLambda.asExprOf[CpsAsyncEmitter[F,T] => F[Unit]]})(using $ctp) 
                     }.asTerm
                  case None =>
                     throw MacroError(s"Can't resolve CpsMonadContextProvider[${TypeRepr.of[F].show}]",f.asExpr)
            case Block(List(), last) => transformTree(last, absorber)
            case Block(stats, last) => Block(stats, transformTree(last, absorber))
            case _ =>
                throw MacroError("lambda expected in asyncStream", f.asExpr)
