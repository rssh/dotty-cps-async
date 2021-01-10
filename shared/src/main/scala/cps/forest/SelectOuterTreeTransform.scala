package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait SelectOuterTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import quotes.reflect._

  // case selectOuterTerm @ SelectOuter(qualifier,name,level) 
  def runSelectOuter( term: SelectOuter ): CpsTree =
     val qual = runRoot(term.qualifier, TransformationContextMarker.SelectOuter)
     if (!qual.isChanged)
        CpsTree.pure(term)
     else
        term match
          case Select(q1, symbol) =>
                 // TODO: TypeApply can live in qual
                 SelectTypeApplyCpsTree(q1,List(),List(SelectTypeApplyRecord(symbol,List(),term.level)))
          case _ =>
                 throw MacroError("Expected that SelectOuter also match Select", posExprs(term))
        //val r = qual.applyTerm1(t => SelectOuter(t, term.name, term.level), term.tpe)
        r


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

