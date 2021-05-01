package cps.forest

import scala.quoted._

import cps._


class TypeApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case TypeApply(fun,targs)
  def run(using Quotes)(fun: quotes.reflect.Term, targs: List[quotes.reflect.TypeTree]): CpsExpr[F,T] =
     import quotes.reflect._
     TypeApplyTreeTransform.run(cpsCtx,patternCode.asTerm, fun, targs)

