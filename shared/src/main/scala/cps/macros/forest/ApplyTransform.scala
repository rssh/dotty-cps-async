// CPS Transform for tasty apply
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021
package cps.macros.forest

import scala.quoted.*

import cps.*
import cps.macros.*


class ApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Apply(fun,args)
  def run(using qctx: Quotes)(fun: quotes.reflect.Term, args: List[quotes.reflect.Term]): CpsExpr[F,T] =
     import quotes.reflect._
     ApplyTreeTransform.run(cpsCtx, patternCode.asTerm, fun, args)

