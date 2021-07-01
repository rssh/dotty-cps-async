// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._


class TypeApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case TypeApply(fun,targs)
  def run(using Quotes)(fun: quotes.reflect.Term, targs: List[quotes.reflect.TypeTree]): CpsExpr[F,T] =
     import quotes.reflect._
     TypeApplyTreeTransform.run(cpsCtx,patternCode.asTerm, fun, targs)

