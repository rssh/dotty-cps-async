package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class TypeApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case TypeApply(fun,targs) 
  def run(given qctx: QuoteContext)(fun: qctx.tasty.Term, targs: List[qctx.tasty.TypeTree]): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
 
     val cpsResult = TypeApplyTreeTransform.run(cpsCtx,patternCode.unseal, fun, targs)
     cpsResult
     

