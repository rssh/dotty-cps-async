package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class TryTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Try(body, cases, finalizer) 
  def run(using qctx: QuoteContext)(body: qctx.tasty.Term, 
                                    cases: List[qctx.tasty.CaseDef],
                                    finalizer: Option[qctx.tasty.Term]): CpsExpr[F,T] = 
     println("try/catch handling")
     import qctx.tasty.{_, given}
     val cpsBody = Async.rootTransform[F,T](body.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, exprMarker+"B")    
     val cpsCaseDefs = cases.zipWithIndex.map((cd,i) => Async.rootTransform[F,T](
                                       cd.rhs.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, exprMarker+i.toString))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.isAsync)
     val optCpsFinalizer = finalizer.map( x => Async.rootTransform[F,Unit](
                                        x.seal.asInstanceOf[Expr[Unit]], asyncMonad, exprMarker+"F"))
     val isFinalizerAsync = optCpsFinalizer.exists(_.isAsync)
     val isAsync = cpsBody.isAsync || isCaseDefsAsync || isFinalizerAsync

     def makeRestoreExpr(): Expr[Throwable => F[T]]  =
        val nCaseDefs = (cases lazyZip cpsCaseDefs) map { (frs,snd) =>
           CaseDef(frs.pattern, frs.guard, snd.transformed.unseal)
        }
        val restoreExpr = '{ (ex: Throwable) => ${Match('ex.unseal, nCaseDefs.toList).seal} }
        restoreExpr.asInstanceOf[Expr[Throwable => F[T]]]

     val builder = if (!isAsync) {
                      CpsExpr.sync(asyncMonad, patternCode) 
                   } else {
                      optCpsFinalizer match 
                        case None =>
                           if (cpsCaseDefs.isEmpty) 
                             cpsBody
                           else 
                             CpsExpr.async[F,T](cpsCtx.asyncMonad,
                               '{
                                 ${cpsCtx.asyncMonad}.restore(
                                   ${cpsBody.transformed}
                                   )(${makeRestoreExpr()})
                               })
                        case Some(cpsFinalizer) =>
                           if (cpsCaseDefs.isEmpty) 
                             CpsExpr.async[F,T](cpsCtx.asyncMonad,
                               '{
                                  ${cpsCtx.asyncMonad}.withAction(
                                    ${cpsBody.transformed}
                                  )(${cpsFinalizer.transformed})
                               })
                           else
                             CpsExpr.async[F,T](cpsCtx.asyncMonad,
                                 '{
                                   ${cpsCtx.asyncMonad}.withAction(
                                    ${cpsCtx.asyncMonad}.restore(
                                     ${cpsBody.transformed}
                                    )(${makeRestoreExpr()})
                                   )(${cpsFinalizer.transformed})
                                 })
                   }
     builder


