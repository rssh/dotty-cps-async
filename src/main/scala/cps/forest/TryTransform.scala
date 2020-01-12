package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class TryTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Try(body, cases, finalizer) 
  def run(given qctx: QuoteContext)(body: qctx.tasty.Term, 
                                    cases: List[qctx.tasty.CaseDef],
                                    finalizer: Option[qctx.tasty.Term]): CpsExprResult[F,T] = 
     println("try/catch handling")
     import qctx.tasty.{_, given}
     val cpsBody = Async.rootTransform[F,T](body.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, false)    
     val cpsCaseDefs = cases.map(cd => Async.rootTransform[F,T](
                                       cd.rhs.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, false))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.haveAwait)
     val optCpsFinalizer = finalizer.map( x => Async.rootTransform[F,Unit](
                                        x.seal.asInstanceOf[Expr[Unit]], asyncMonad, false))
     val isFinalizerAsync = optCpsFinalizer.exists(_.haveAwait)
     val isAsync = cpsBody.haveAwait || isCaseDefsAsync || isFinalizerAsync

     val builder = if (!isAsync) {
                      CpsChunkBuilder.sync(patternCode,asyncMonad) 
                   } else {
                      optCpsFinalizer match 
                        case None =>
                           if (cpsCaseDefs.isEmpty) 
                             cpsBody.chunkBuilder
                           else 
                             CpsChunkBuilder.async[F,T](cpsCtx.asyncMonad,
                               '{
                                 ${cpsCtx.asyncMonad}.restore(
                                   ${cpsBody.transformed}
                                   )(${makeRestoreExpr(cpsCaseDefs)})
                               })
                        case Some(cpsFinalizer) =>
                           if (cpsCaseDefs.isEmpty) 
                             CpsChunkBuilder.async[F,T](cpsCtx.asyncMonad,
                               '{
                                  ${cpsCtx.asyncMonad}.withAction(
                                    ${cpsBody.transformed}
                                  )(${cpsFinalizer.transformed})
                               })
                           else
                             CpsChunkBuilder.async[F,T](cpsCtx.asyncMonad,
                                 '{
                                   ${cpsCtx.asyncMonad}.withAction(
                                    ${cpsCtx.asyncMonad}.restore(
                                     ${cpsBody.transformed}
                                    )(${makeRestoreExpr(cpsCaseDefs)})
                                   )(${cpsFinalizer.transformed})
                                 })
                   }
     CpsExprResult(patternCode, builder, patternType, isAsync)

  def makeRestoreExpr[F[_]:Type,T:Type](given qctx: QuoteContext)(
                                        caseDefs: List[CpsExprResult[F,T]]):Expr[Throwable => F[T]] =
     import qctx.tasty.{_, given}
     ???



