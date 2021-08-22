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
 * Emitter which should be a parameter of asyncStream expression.
 *
 * ```
 *   asyncStream[AsyncList[F,Int]] { out =>
 *      for(i <- 1 to 10) 
 *         out.emit(i)
 *   }
 * ```
 * Here out have a `CpsAsyncEmitter[AsyncList[F,Int],F,E]` type.
 **/
trait CpsAsyncEmitter[R, F[_]: CpsAsyncMonad, E]:

   transparent inline def emit(v:E): Unit =
      await(emitAsync(v))

   def emitAsync(v:E): F[Unit]



trait CpsAsyncEmitAbsorber[R]:

   type Monad[_]
   type Element

   def evalAsync(f: CpsAsyncEmitter[R,Monad,Element] => Monad[Unit]): R

   def asyncMonad: CpsAsyncMonad[Monad]


object CpsAsyncEmitAbsorber:

   type Aux[R, F[_],T] = CpsAsyncEmitAbsorber[R]  {

      type Monad[X] = F[X]
      type Element = T
       
   }


trait CpsAsyncEmitAbsorber3[R, F[_]: CpsAsyncMonad, T] extends CpsAsyncEmitAbsorber[R]:

   override type Element = T
   override type Monad[X] = F[X]


   
transparent inline def asyncStream[R](using a: CpsAsyncEmitAbsorber[R]) =
     AsyncStreamHelper(a)




class AsyncStreamHelper[R,F[_],A](a: CpsAsyncEmitAbsorber.Aux[R,F,A]){

    transparent inline def apply(inline f: CpsAsyncEmitter[R,F,A] => Unit): R = ${
         CpsAsyncStreamMacro.transform[R,F,A]('f, 'a)
    }
       

}

object CpsAsyncStreamMacro:

    def transform[R:Type, F[_]:Type, T:Type](f: Expr[CpsAsyncEmitter[R,F,T] => Unit], 
                                             absorber: Expr[CpsAsyncEmitAbsorber.Aux[R,F,T]])(using Quotes): Expr[R] =
          import quotes.reflect._
          //val r = transformTree[R,F,T](f.asTerm, absorber.asExprOf[CpsAsyncEmitAbsorber.Aux[R,F,T]])
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
                    val param = params.head
                    val mt = MethodType(List(param.name))(_ => List(param.tpt.tpe), _ => TypeRepr.of[F].appliedTo(body.tpe.widen))
                    val nLambda = Lambda(Symbol.spliceOwner, mt, (symbol, args) => {
                        val argTerm = args.head.asInstanceOf[Term]

                        val nBody = cps.macros.forest.TransformUtil.substituteLambdaParams(params,args, body, symbol)
                        val asyncBody = cps.macros.Async.transformMonad[F,Unit](nBody.asExprOf[Unit], '{ ${absorber}.asyncMonad }).asTerm
                        asyncBody.changeOwner(symbol)
                    })
                    '{ ${absorber}.evalAsync(${nLambda.asExprOf[CpsAsyncEmitter[R,F,T] => F[Unit]]}) }.asTerm
            case Block(List(), last) => transformTree(last, absorber)
            case Block(stats, last) => Block(stats, transformTree(last, absorber))
            case _ =>
                throw MacroError("lambda expected in asyncStream", f.asExpr)
