package cps.forest

import cps._
import scala.quoted._

trait TreeTransformScope[F[_]:Type,CT:Type]
               extends CpsTreeScope[F, CT]
                  with KnownTreeFragments[F,CT]
                  with TypeApplyTreeTransform[F,CT]
                  with RootTreeTransform[F, CT]
                  with ApplyTreeTransform[F,CT]
                  with AwaitTreeTransform[F, CT]
                  with SelectTreeTransform[F, CT]
                  with LambdaTreeTransform[F, CT]
                  with MatchTreeTransform[F, CT]
                  with AsyncTreeShifter[F,CT]
{

   val cpsCtx: TransformationContext[F,CT]

   implicit val qctx: QuoteContext

   implicit val fType: quoted.Type[F]

   implicit val ctType: quoted.Type[CT]

}


trait TreeTransformScopeInstance[F[_]:Type,T:Type](
         override val cpsCtx: TransformationContext[F,T])
         (implicit override val qctx: QuoteContext)
                                 extends TreeTransformScope[F,T] {


}

