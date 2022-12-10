package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait SelectOuterTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import quotes.reflect._

  // case selectOuterTerm @ SelectOuter(qualifier,name,level) 
  def runSelectOuter( term: SelectOuter )(owner: Symbol): CpsTree =
     val qual = runRoot(term.qualifier)(owner)
     if (!qual.isChanged)
        // TODO: mb not use pure ?
        CpsTree.pure(owner,term)
     else
        SelectTypeApplyCpsTree.create(Some(term), qual,List(),
           List(SelectTypeApplyRecord(term.qualifier.tpe, term.symbol,List(),term.level)),term.tpe)


object SelectOuterTreeTransform:


  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         selectOuterTerm: qctx1.reflect.SelectOuter): CpsExpr[F,T] = {
                         
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
            val origin = selectOuterTerm.asInstanceOf[quotes.reflect.SelectOuter]
            val owner = quotes.reflect.Symbol.spliceOwner
            runSelectOuter(origin)(owner).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

