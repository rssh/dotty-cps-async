package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait SelectTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_, given _}

  // case selectTerm @ Select(qualifier,name) 
  def runSelect( selectTerm: Select ): CpsTree =
     val symbol = selectTerm.symbol
     runRoot(selectTerm.qualifier).applyTerm(_.select(symbol), selectTerm.tpe)


object SelectTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         selectTerm: qctx1.tasty.Select): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            val origin = selectTerm.asInstanceOf[qctx.tasty.Select]
            runSelect(origin).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

