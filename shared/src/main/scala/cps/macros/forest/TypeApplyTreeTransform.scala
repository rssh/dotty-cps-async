package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait TypeApplyTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  // case TypeApply(fun,targs) 
  def runTypeApply( applyTerm: qctx.reflect.Term, 
                    fun: qctx.reflect.Term, 
                    targs: List[qctx.reflect.TypeTree]): CpsTree =
     runRoot(fun).typeApply(applyTerm, targs, applyTerm.tpe)


object TypeApplyTreeTransform:


  def run[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         applyTerm: qctx1.reflect.Term, 
                         fun: qctx1.reflect.Term, 
                         targs: List[qctx1.reflect.TypeTree]): CpsExpr[F,T] = {
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
            runTypeApply(applyTerm.asInstanceOf[quotes.reflect.Term],
                         fun.asInstanceOf[quotes.reflect.Term],
                         targs.asInstanceOf[List[quotes.reflect.TypeTree]]
                        ).toResult[T].asInstanceOf[CpsExpr[F,T]]

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

