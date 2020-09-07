package cps.forest

import scala.quoted._

import cps.{TransformationContextMarker=>TCM,_}
import cps.misc._


class TryTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Try(body, cases, finalizer)
  def run(using qctx: QuoteContext)(body: qctx.tasty.Term,
                                    cases: List[qctx.tasty.CaseDef],
                                    finalizer: Option[qctx.tasty.Term]): CpsExpr[F,T] =
     import qctx.tasty._
     val cpsBody = Async.nestTransform(body.seal.cast[T],
                                            cpsCtx, TCM.TryBody)
     val cpsCaseDefs = cases.zipWithIndex.map((cd,i) => Async.nestTransform(
                                                  cd.rhs.seal.cast[T],
                                                  cpsCtx, TCM.TryCase(i)))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.isAsync)
     val optCpsFinalizer = finalizer.map( x => Async.nestTransform[F,T,Unit](
                                        x.seal.cast[Unit], cpsCtx, TCM.TryFinally))
     val isFinalizerAsync = optCpsFinalizer.exists(_.isAsync)
     val isAsync = cpsBody.isAsync || isCaseDefsAsync || isFinalizerAsync

     def makeRestoreExpr(): Expr[Throwable => F[T]]  =
        val nCaseDefs = (cases lazyZip cpsCaseDefs) map { (frs,snd) =>
           CaseDef(frs.pattern, frs.guard, snd.transformed.unseal)
        }
        val restoreExpr = '{ (ex: Throwable) => ${Match('ex.unseal, nCaseDefs.toList).seal.cast[F[T]]} }
        restoreExpr.cast[Throwable => F[T]]



     val builder = if (!isAsync) {
                      CpsExpr.sync(monad, patternCode)
                   } else {
                      val errorMonad = if (monad.unseal.tpe <:< Type.of[CpsTryMonad[F]]) {
                                          monad.cast[CpsTryMonad[F]]
                                      } else {
                                          throw MacroError(s"${monad} should be instance of CpsTryMonad for try/catch support", patternCode)
                                      }
                      optCpsFinalizer match
                        case None =>
                           if (cpsCaseDefs.isEmpty)
                             cpsBody
                           else
                             CpsExpr.async[F,T](cpsCtx.monad,
                               '{
                                 ${errorMonad}.restore(
                                   ${cpsBody.transformed}
                                   )(${makeRestoreExpr()})
                               })
                        case Some(cpsFinalizer) =>
                           if (cpsCaseDefs.isEmpty)
                             CpsExpr.async[F,T](cpsCtx.monad,
                               '{
                                  ${errorMonad}.withAction(
                                    ${cpsBody.transformed}
                                  )(${cpsFinalizer.transformed})
                               })
                           else
                             CpsExpr.async[F,T](cpsCtx.monad,
                                 '{
                                   ${errorMonad}.withAction(
                                    ${errorMonad}.restore(
                                     ${cpsBody.transformed}
                                    )(${makeRestoreExpr()})
                                   )(${cpsFinalizer.transformed})
                                 })
                   }
     builder


