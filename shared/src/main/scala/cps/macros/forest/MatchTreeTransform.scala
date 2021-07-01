// transform for match
//  (C) Ruslan Shevchenko, 2019-2021, Kiev, Ukraine
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait MatchTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  // case selectTerm @ Select(qualifier,name) 
  def runMatch( matchTerm: Match ): CpsTree =
     if (cpsCtx.flags.debugLevel >= 15) then
         cpsCtx.log(s"matchTransform: matchTerm.tpe=${matchTerm.tpe}")
         cpsCtx.log(s"matchTransform: matchTerm.tpe.widen=${matchTerm.tpe.widen}")
         cpsCtx.log(s"matchTransform: veryWiden(matchTerm.tpe)=${TransformUtil.veryWiden(matchTerm.tpe)}")
     //val widenOtpe = TransformUtil.veryWiden(matchTerm.tpe)
     //val otpe = widenOtpe
     val otpe = matchTerm.tpe
     val scrutinee = matchTerm.scrutinee
     val cpsScrutinee = runRoot(scrutinee)
     val cpsCases = matchTerm.cases.map( caseDef => runRoot(caseDef.rhs) )
     val asyncCases = cpsCases.exists( _.isAsync )
     val nCases = if (asyncCases) {
                      (matchTerm.cases zip cpsCases).map{(old,cpstree) =>
                         CaseDef.copy(old)(old.pattern, old.guard, cpstree.transformed)
                      }   
                   } else {
                      matchTerm.cases
                   }  
     if (!cpsScrutinee.isAsync) 
        if (!asyncCases) 
           CpsTree.pure(matchTerm)
        else 
           val nTree = Match.copy(matchTerm)(scrutinee, nCases)
           CpsTree.impure(nTree, otpe)
     else
        if (!asyncCases) 
           cpsScrutinee.monadMap( x => Match(x, nCases), otpe )
        else
           cpsScrutinee.monadFlatMap( x => Match(x, nCases), otpe )


   

object MatchTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         matchTerm: qctx1.reflect.Match): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            val origin = matchTerm.asInstanceOf[quotes.reflect.Match]
            runMatch(origin).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

