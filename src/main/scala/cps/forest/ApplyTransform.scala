// CPS Transform for tasty apply
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class ApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Apply(fun,args) 
  def run(using qctx: QuoteContext)(fun: qctx.tasty.Term, args: List[qctx.tasty.Term]): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     ApplyTreeTransform.run(cpsCtx, patternCode.unseal, fun, args)
     

