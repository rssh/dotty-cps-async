package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait LambdaTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  def typeInMonad(tp:TypeRepr): TypeRepr =
       fType.unseal.tpe.appliedTo(tp)

  // case lambdaTree @ Lambda(params,body)
  def runLambda(lambdaTerm: Term, params: List[ValDef], expr: Term ): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log(s"runLambda, lambda=${safeShow(lambdaTerm)}")
       cpsCtx.log(s"runLambda, expr=${safeShow(expr)}")
     val cpsBody = runRoot(expr,TransformationContextMarker.Lambda)
     val retval = if (cpsBody.isAsync) {
        // in general, shifted lambda
        if (cpsCtx.flags.allowShiftedLambda) then
            AsyncLambdaCpsTree(lambdaTerm, params, cpsBody, lambdaTerm.tpe)
        else
            throw MacroError("await inside lambda functions without enclosing async block", lambdaTerm.seal)
     } else {
        CpsTree.pure(lambdaTerm)
     }
     retval


  def shiftedMethodType(paramNames: List[String], paramTypes:List[TypeRepr], otpe: TypeRepr): MethodType =
     MethodType(paramNames)(_ => paramTypes, _ => typeInMonad(otpe.widen))




object LambdaTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         lambdaTerm: qctx1.reflect.Term,
                         params: List[qctx1.reflect.ValDef],
                         expr: qctx1.reflect.Term): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = lambdaTerm.asInstanceOf[qctx.reflect.Term]
            val xparams = params.asInstanceOf[List[qctx.reflect.ValDef]]
            val xexpr   = expr.asInstanceOf[qctx.reflect.Term]
            runLambda(origin, xparams, xexpr).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }


