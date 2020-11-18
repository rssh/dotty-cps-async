package cps.forest

import scala.quoted._

import cps._


class TypeApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case TypeApply(fun,targs) 
  def run(using qctx: QuoteContext)(fun: qctx.reflect.Term, targs: List[qctx.reflect.TypeTree]): CpsExpr[F,T] =
     import qctx.reflect._
     TypeApplyTreeTransform.run(cpsCtx,Term.of(patternCode), fun, targs)
     

