package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait SelectOuterTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import quotes.reflect._

  // case selectOuterTerm @ SelectOuter(qualifier,name,level) 
  def runSelectOuter( term: SelectOuter ): CpsTree =
     val qual = runRoot(term.qualifier)
     if (!qual.isChanged)
        // TODO: mb not use pure ?
        CpsTree.pure(term)
     else
        SelectTypeApplyCpsTree.create(Some(term), qual,List(),
           List(SelectTypeApplyRecord(term.qualifier.tpe, term.symbol,List(),term.level)),term.tpe)


object SelectOuterTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         selectOuterTerm: qctx1.reflect.SelectOuter): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            val origin = selectOuterTerm.asInstanceOf[quotes.reflect.SelectOuter]
            runSelectOuter(origin).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

