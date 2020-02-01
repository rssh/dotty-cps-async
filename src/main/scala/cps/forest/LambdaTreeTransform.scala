package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait LambdaTreeTransform[F[_]]

  thisScope: TreeTransformScope[F] =>

  import qctx.tasty.{_, given}

  // case lambdaTree @ Lambda(params,body) 
  def runLambda(lambdaTerm: Term, params: List[ValDef], expr: Term ): CpsTree =
     val symbol = lambdaTerm.symbol
     val tpe = lambdaTerm.tpe
     System.out.println(s"Lambda, symbol=${symbol}, tpe=${tpe}")
     lambdaTerm.tpe match 
        case x: MethodType => System.out.println("MethodType detected")
        case other =>
          if (other.isFunctionType)
               System.out.println("Function Type (not method type)")
               System.out.println(s"${tpe}")
     val cpsBody = runRoot(expr)
     if (cpsBody.isAsync) {
        buildShiftedLambda(lambdaTerm, params, cpsBody)
     } else {
        CpsTree.pure(lambdaTerm)
     }

  def buildShiftedLambda(lambdaTerm: Term, params: List[ValDef], cpsBody: CpsTree): CpsTree =
    val methodType = buildShiftedMethodType(params, cpsBody.otpe)
    ???
     

  def buildShiftedMethodType(params: List[ValDef], otpe: Type): MethodType =
     val paramNames = params.map{ case ValDef(name,_,_) => name  }
     var paramIndexes: Map[Symbol, Int] = Map.empty
     var paramsSubst: Map[Symbol, Int] = Map.empty
     for((valDef,i) <- params.zipWithIndex) {
       println("ValDef:"+valDef)
       val tpt = valDef.tpt
       paramsSubst = paramsSubst.updated( valDef.symbol, i)
     }

     def shiftedParamType(tp: Type): Type = 
         if (tp.isFunctionType) 
            ???
         else 
            tp

     //TODO: change param name for shifted functions.
     //  (now they are not implemented)
     MethodType(paramNames)(mt => {
                 ??? 
                }, ???)


  end buildShiftedMethodType
     


object LambdaTreeTransform


  def run[F[_]:Type,T:Type](given qctx: QuoteContext)(cpsCtx: TransformationContext[F,T],
                         lambdaTerm: qctx.tasty.Term,
                         params: List[qctx.tasty.ValDef],
                         expr: qctx.tasty.Term): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
          
         def bridge(): CpsExpr[F,T] =
            val origin = lambdaTerm.asInstanceOf[qctx.tasty.Term]
            val xparams = params.asInstanceOf[List[qctx.tasty.ValDef]]
            val xexpr   = expr.asInstanceOf[qctx.tasty.Term]
            runLambda(origin, xparams, xexpr).toResult(cpsCtx.patternCode).asInstanceOf[CpsExpr[F,T]]
                        

     } 
     (new Bridge(cpsCtx)).bridge()
  }


