package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait SelectTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import quotes.reflect._

  // case selectTerm @ Select(qualifier,name) 
  def runSelect( selectTerm: Select ): CpsTree =
     val symbol = selectTerm.symbol
     val qual = runRoot(selectTerm.qualifier)
     val r = qual.select(selectTerm, symbol, selectTerm.tpe)
     r


object SelectTreeTransform:


  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         selectTerm: qctx1.reflect.Select): CpsExpr[F,T] = {
                         
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
            val origin = selectTerm.asInstanceOf[quotes.reflect.Select]
            runSelect(origin).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

