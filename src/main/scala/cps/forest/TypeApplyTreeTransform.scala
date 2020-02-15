package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait TypeApplyTreeTransform[F[_]]:

  thisScope: TreeTransformScope[F] =>

  import qctx.tasty.{_, given _}

  // case TypeApply(fun,targs) 
  def runTypeApply( applyTerm: qctx.tasty.Term, 
                    fun: qctx.tasty.Term, 
                    targs: List[qctx.tasty.TypeTree]): CpsTree =
     runRoot(fun).typeApply(targs, applyTerm.tpe)


object TypeApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx: QuoteContext)(cpsCtx: TransformationContext[F,T],
                         applyTerm: qctx.tasty.Term, 
                         fun: qctx.tasty.Term, 
                         targs: List[qctx.tasty.TypeTree]): CpsExpr[F,T] = {
     val tmpFType = summon[Type[F]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
          
         def bridge(): CpsExpr[F,T] =
            runTypeApply(applyTerm.asInstanceOf[qctx.tasty.Term],
                         fun.asInstanceOf[qctx.tasty.Term],
                         targs.asInstanceOf[List[qctx.tasty.TypeTree]]
                        ).toResult(cpsCtx.patternCode).asInstanceOf[CpsExpr[F,T]]

     } 
     (new Bridge(cpsCtx)).bridge()
  }

