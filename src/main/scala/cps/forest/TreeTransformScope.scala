package cps.forest

import cps._
import scala.quoted._
import scala.tasty._

trait TreeTransformScope[F[_]:Type]
               extends CpsTreeScope[F]
                  with KnownTreeFragments[F]
                  with TypeApplyTreeTransform[F]
                  with RootTreeTransform[F]
                  with ApplyTreeTransform[F]
                  with AwaitTreeTransform[F]
                  with SelectTreeTransform[F]
                  with LambdaTreeTransform[F]
                  with RepeatedTreeTransform[F]
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

