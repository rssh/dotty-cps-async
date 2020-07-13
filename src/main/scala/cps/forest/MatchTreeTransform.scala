package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait MatchTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_, given _}

  // case selectTerm @ Select(qualifier,name) 
  def runMatch( matchTerm: Match ): CpsTree =
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
           CpsTree.impure(nTree, matchTerm.tpe)
     else
        if (!asyncCases) 
           cpsScrutinee.monadMap( x => Match(x, nCases), matchTerm.tpe )
        else
           cpsScrutinee.monadFlatMap( x => Match(x, nCases), matchTerm.tpe )


object MatchTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         matchTerm: qctx1.tasty.Match): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            val origin = matchTerm.asInstanceOf[qctx.tasty.Match]
            runMatch(origin).toResult[T]
                        

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

