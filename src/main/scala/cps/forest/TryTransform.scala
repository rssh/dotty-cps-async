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
     import qctx.tasty.{_, given}
     println("try/catch handling")
     import qctx.tasty.{_, given}
     val cpsBody = Async.rootTransform[F,T](body.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, false)    
     val cpsCaseDefs = cases.map(cd => Async.rootTransform[F,T](
                                       cd.rhs.seal.asInstanceOf[Expr[T]],
                                       asyncMonad, false))
     val isCaseDefsAsync = cpsCaseDefs.exists(_.haveAwait)
     
     val optCpsFinalizer = finalizer.map( x => Async.rootTransform[F,T](
                                            x.seal.asInstanceOf[Expr[T]], asyncMonad, false))
     val isFinalizerAsync = optCpsFinalizer.exists(_.haveAwait)

     val builder = if (!cpsBody.haveAwait && !isCaseDefsAsync && ! isFinalizerAsync) {
                      CpsChunkBuilder.sync(patternCode,asyncMonad) 
                   } else if (isCaseDefsAsync) {
                      ???
                   } else {
                      ???
                   }
                                    
     CpsExprResult(patternCode, builder, patternType, cpsBody.haveAwait || isCaseDefsAsync | isFinalizerAsync)


