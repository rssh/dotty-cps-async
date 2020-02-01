package cps.forest

import cps._
import scala.quoted._
import scala.tasty._

trait TreeTransformScope[F[_]:Type]
               extends CpsTreeScope[F]
                  with RootTreeTransform[F]
                  with ApplyTreeTransform[F]
{

   val cpsCtx: TransformationContext[F,?]

   implicit val qctx: QuoteContext

   implicit val fType: quoted.Type[F]

}


trait TreeTransformScopeInstance[F[_]:Type,T:Type](
         override val cpsCtx: TransformationContext[F,T])
         (implicit override val qctx: QuoteContext)
                                 extends TreeTransformScope[F] {


}

