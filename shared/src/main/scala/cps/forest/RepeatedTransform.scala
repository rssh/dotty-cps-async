package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class RepeatedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // TODO: implement
  def run(using Quotes)(repeated: quotes.reflect.Repeated): CpsExpr[F,T] =
      RepeatedTreeTransform.run(cpsCtx, repeated, repeated.elems, repeated.elemtpt)



  
