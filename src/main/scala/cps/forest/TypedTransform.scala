package cps.forest

import scala.quoted._

import cps._


class TypedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(t: qctx.tasty.Term, tp: qctx.tasty.TypeTree): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     val r = Async.nestTransform(t.seal.asInstanceOf[Expr[T]], cpsCtx, TransformationContextMarker.Typed)
     if (!r.isAsync) 
       CpsExpr.sync(monad, patternCode)
     else
       // TODO:  create Typed with F[$tp] as type ?
       r
  


