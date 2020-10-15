package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait TypeApplyTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  // case TypeApply(fun,targs) 
  def runTypeApply( applyTerm: qctx.reflect.Term, 
                    fun: qctx.reflect.Term, 
                    targs: List[qctx.reflect.TypeTree]): CpsTree =
     runRoot(fun,TransformationContextMarker.TypeApplyFun).typeApply(targs, applyTerm.tpe)


object TypeApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         applyTerm: qctx1.reflect.Term, 
                         fun: qctx1.reflect.Term, 
                         targs: List[qctx1.reflect.TypeTree]): CpsExpr[F,T] = {
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            runTypeApply(applyTerm.asInstanceOf[qctx.reflect.Term],
                         fun.asInstanceOf[qctx.reflect.Term],
                         targs.asInstanceOf[List[qctx.reflect.TypeTree]]
                        ).toResult[T].asInstanceOf[CpsExpr[F,T]]

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

