package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._
import cps.macros.forest.application.ApplicationShiftType


trait LambdaTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import quotes.reflect._

  // case lambdaTree @ Lambda(params,body)
  def runLambda(lambdaTerm: Term, params: List[ValDef], expr: Term ): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log(s"runLambda, lambda=${safeShow(lambdaTerm)}")
       cpsCtx.log(s"runLambda, expr=${safeShow(expr)}")
     // TODO:  push cpsCtx is this is context function.  
     val cpsBody = runRoot(expr)
     val retval = if (cpsBody.isAsync) {
        // in general, shifted lambda
        if (cpsCtx.flags.allowShiftedLambda) then
            AsyncLambdaCpsTree(lambdaTerm, params, cpsBody, lambdaTerm.tpe)
        else
            throw MacroError("await inside lambda functions without enclosing async block", lambdaTerm.asExpr)
     } else {
        CpsTree.pure(lambdaTerm)
     }
     retval


  /**
  *  method type for (p1 ... pn) => F[otpe]
  **/   
  def cpsShiftedMethodType(paramNames: List[String], paramTypes:List[TypeRepr], otpe: TypeRepr): MethodType =
     MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[F].appliedTo(otpe.widen))
                                          




object LambdaTreeTransform:


  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         lambdaTerm: qctx1.reflect.Term,
                         params: List[qctx1.reflect.ValDef],
                         expr: qctx1.reflect.Term): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     val tmpCCType = summon[Type[C]]
     class Bridge(tc:TransformationContext[F,T,C]) extends
                                                    TreeTransformScope[F,T,C]
                                                    with TreeTransformScopeInstance[F,T,C](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
         implicit val ccType: quoted.Type[C] = tmpCCType

         def bridge(): CpsExpr[F,T] =
            val origin = lambdaTerm.asInstanceOf[quotes.reflect.Term]
            val xparams = params.asInstanceOf[List[quotes.reflect.ValDef]]
            val xexpr   = expr.asInstanceOf[quotes.reflect.Term]
            runLambda(origin, xparams, xexpr).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }


