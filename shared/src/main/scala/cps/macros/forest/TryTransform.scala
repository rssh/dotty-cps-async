package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

import scala.util.control.NonFatal



class TryTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._



  // TODO: create TreeTransform
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
     val optCpsFinalizer = finalizer.map( x => Async.nestTransform[F,T,C,Unit](
                                        x.asExprOf[Unit], cpsCtx ))
     val isFinalizerAsync = optCpsFinalizer.exists(_.isAsync)
     val isFinalizerChanged = optCpsFinalizer.exists(_.isChanged)
     val isAsync = cpsBody.isAsync || isCaseDefsAsync || isFinalizerAsync
     val isChanged = cpsBody.isChanged || isCaseDefsChanged || isFinalizerChanged

     /*
     val errorMonad = if (monad.asTerm.tpe <:< TypeRepr.of[CpsTryMonad[F]]) {
                          monad.asExprOf[CpsTryMonad[F]]
                      } else {
                          throw MacroError(s"${monad} should be instance of CpsTryMonad for try/catch support", patternCode)
                      }
     */
     val errorMonad = try {
       monad.asExprOf[CpsTryMonad[F]]
     } catch {
       case NonFatal(ex) =>
         //report.error(s"${monad} should be instance of CpsTryMonad for try/catch support: ${ex.getMessage}", patternCode)
         report.error(
          s"""|monad=${monad.show},  C=${TypeRepr.of[C].show}, monadContext=${cpsCtx.monadContext.show}
              |monad.tpe.widen=${monad.asTerm.tpe.widen.show},  monadContext.tpw.widen=${cpsCtx.monadContext.asTerm.tpe.widen}
              |monadContext.tpe <:< TypeReprOf[CpsTryMonadContext[F]] = ${monadContext.asTerm.tpe.widen <:< TypeRepr.of[CpsMonadContext]}
          """.stripMargin('|'), patternCode)
         throw MacroError(s"${monad} should be instance of CpsTryMonad for try/catch support: ${ex.getMessage}", patternCode)
     }

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
        //val otherSym = Symbol.newBind(Symbol.spliceOwner,"other",Flags.Empty,TypeRepr.of[Throwable])
        val rethrowOther = (x: Expr[Throwable]) => CaseDef(Wildcard(), None, { val r = '{throw $x}.asTerm; println(s"!!throw=$r"); r } )
        val restoreExpr = '{ (ex: Throwable) => ${
          Match('ex.asTerm, 
                nCaseDefs.appended(CaseDef(Wildcard(),None, '{ $errorMonad.error[T](ex) }.asTerm ))
          ).changeOwner(Symbol.spliceOwner).asExprOf[F[T]]} }
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


