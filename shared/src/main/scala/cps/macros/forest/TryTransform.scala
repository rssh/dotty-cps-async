package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


class TryTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Try(body, cases, finalizer)
  def run(using Quotes)(origin: quotes.reflect.Try,
                        body: quotes.reflect.Term,
                        cases: List[quotes.reflect.CaseDef],
                        finalizer: Option[quotes.reflect.Term]): CpsExpr[F,T] =
     import quotes.reflect._
     val cpsBody = Async.nestTransform(body.asExprOf[T],
                                            cpsCtx)
     val cpsCaseDefs = cases.zipWithIndex.map((cd,i) => Async.nestTransform(
                                                  cd.rhs.asExprOf[T],
                                                  cpsCtx))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.isAsync)
     val isCaseDefsChanged = cpsCaseDefs.exists(_.isChanged)
     val optCpsFinalizer = finalizer.map( x => Async.nestTransform[F,T,Unit](
                                        x.asExprOf[Unit], cpsCtx ))
     val isFinalizerAsync = optCpsFinalizer.exists(_.isAsync)
     val isFinalizerChanged = optCpsFinalizer.exists(_.isChanged)
     val isAsync = cpsBody.isAsync || isCaseDefsAsync || isFinalizerAsync
     val isChanged = cpsBody.isChanged || isCaseDefsChanged || isFinalizerChanged

     def makeAsyncCaseDefs(): List[CaseDef] =
        ((cases lazyZip cpsCaseDefs) map { (frs,snd) =>
           CaseDef(frs.pattern, frs.guard, snd.transformed.asTerm)
        }).toList

     def makeSyncCaseDefs(): List[CaseDef] =
        ((cases lazyZip cpsCaseDefs) map { (frs,snd) =>
           CaseDef(frs.pattern, frs.guard, snd.syncOrigin.get.asTerm)
        }).toList

     def makeRestoreExpr(): Expr[Throwable => F[T]]  =
        val nCaseDefs = makeAsyncCaseDefs()
        val restoreExpr = '{ (ex: Throwable) => ${Match('ex.asTerm, nCaseDefs).asExprOf[F[T]]} }
        restoreExpr.asExprOf[Throwable => F[T]]


     val builder = if (!isAsync) {
                      if (!isChanged) {
                         CpsExpr.sync(monad, patternCode, false)
                      } else {
                         val expr = quotes.reflect.Try.copy(origin)(
                                            cpsBody.syncOrigin.get.asTerm, 
                                            makeSyncCaseDefs(),
                                            optCpsFinalizer.map(_.syncOrigin.get.asTerm)
                         ).asExprOf[T]
                         CpsExpr.sync(monad, expr, true)
                      }
                   } else {
                      val errorMonad = if (monad.asTerm.tpe <:< TypeRepr.of[CpsTryMonad[F]]) {
                                          monad.asExprOf[CpsTryMonad[F]]
                                      } else {
                                          throw MacroError(s"${monad} should be instance of CpsTryMonad for try/catch support", patternCode)
                                      }
                      optCpsFinalizer match
                        case None =>
                           if (cpsCaseDefs.isEmpty)
                             cpsBody
                           else
                             cpsBody.syncOrigin match
                               case None =>
                                 CpsExpr.async[F,T](cpsCtx.monad,
                                  '{
                                     ${errorMonad}.restore(
                                       ${errorMonad}.tryImpure(
                                         ${cpsBody.transformed}
                                       )
                                      )(${makeRestoreExpr()})
                                  })
                               case Some(syncBody) =>
                                 val nBody = '{ ${monad}.pure($syncBody) }.asTerm
                                 CpsExpr.async[F,T](cpsCtx.monad,
                                    Try(nBody, makeAsyncCaseDefs(), None).asExprOf[F[T]]
                                 )
                        case Some(cpsFinalizer) =>
                           if (cpsCaseDefs.isEmpty)
                             cpsBody.syncOrigin match
                               case None =>
                                 cpsFinalizer.syncOrigin match
                                   case Some(syncFinalizer) =>
                                      CpsExpr.async[F,T](cpsCtx.monad,
                                       '{
                                         ${errorMonad}.withAction(
                                            ${errorMonad}.tryImpure(
                                              ${cpsBody.transformed}
                                            )
                                         )(${syncFinalizer})
                                      })
                                   case None =>
                                      CpsExpr.async[F,T](cpsCtx.monad,
                                       '{
                                         ${errorMonad}.withAsyncAction(
                                            ${errorMonad}.tryImpure(
                                               ${cpsBody.transformed}
                                            )
                                         )(${cpsFinalizer.transformed})
                                      })
                               case Some(syncBody) =>
                                 cpsFinalizer.syncOrigin match
                                   case Some(syncFinalizer) =>
                                      CpsExpr.async[F,T](cpsCtx.monad,
                                       '{
                                         ${errorMonad}.withAction(
                                           ${errorMonad}.tryPure($syncBody)
                                         )(${syncFinalizer})
                                      })
                                   case None =>
                                      CpsExpr.async[F,T](cpsCtx.monad,
                                       '{
                                         ${errorMonad}.withAsyncAction(
                                           ${errorMonad}.tryPure($syncBody)
                                         )(${cpsFinalizer.transformed})
                                      })
                           else
                             cpsBody.syncOrigin match
                               case Some(syncBody) =>
                                 cpsFinalizer.syncOrigin match
                                   case Some(syncFinalizer) =>
                                     CpsExpr.async[F,T](cpsCtx.monad,
                                      '{
                                         ${errorMonad}.withAction(
                                           ${errorMonad}.restore(
                                             ${errorMonad}.tryPure($syncBody)
                                           )(${makeRestoreExpr()})
                                         )($syncFinalizer)
                                     })
                                   case None =>
                                     CpsExpr.async[F,T](cpsCtx.monad,
                                      '{
                                         ${errorMonad}.withAsyncAction(
                                           ${errorMonad}.restore(
                                             ${errorMonad}.tryPure($syncBody)
                                           )(${makeRestoreExpr()})
                                         )(${cpsFinalizer.transformed})
                                      })
                               case None =>
                                 cpsFinalizer.syncOrigin match
                                   case Some(syncFinalizer) =>
                                     CpsExpr.async[F,T](cpsCtx.monad,
                                      '{
                                         ${errorMonad}.withAction(
                                           ${errorMonad}.restore(
                                             ${errorMonad}.tryImpure(
                                               ${cpsBody.transformed}
                                             )
                                           )(${makeRestoreExpr()})
                                         )($syncFinalizer)
                                     })
                                   case None =>
                                     CpsExpr.async[F,T](cpsCtx.monad,
                                      '{
                                         ${errorMonad}.withAsyncAction(
                                           ${errorMonad}.restore(
                                             ${errorMonad}.tryImpure(
                                               ${cpsBody.transformed}
                                             )
                                           )(${makeRestoreExpr()})
                                         )(${cpsFinalizer.transformed})
                                     })
                   }
     builder


