package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait MatchTreeTransform[F[_]]:

  thisScope: TreeTransformScope[F] =>

  import qctx.tasty.{_, given _}

  // case selectTerm @ Select(qualifier,name) 
  def runMatch( matchTerm: Match ): CpsTree =
     val scrutinee = matchTerm.scrutinee
     val cpsScrutinee = runRoot(scrutinee)
     val cpsCases = matchTerm.cases.map( caseDef => runRoot(caseDef.rhs) )
     val asyncCases = cpsCases.exists( _.isAsync )
     val nCases = if (asyncCases) {
                      (matchTerm.cases zip cpsCases).map{(old,cpstree) =>
                         CaseDef(old.pattern, old.guard, cpstree.transformed)
                      }   
                   } else {
                      matchTerm.cases
                   }  
     if (!cpsScrutinee.isAsync) 
        if (!asyncCases) 
           CpsTree.pure(matchTerm)
        else 
           val nTree = Match(scrutinee, nCases)
           CpsTree.impure(nTree, matchTerm.tpe)
     else
        if (!asyncCases) 
           cpsScrutinee.monadMap( x => Match(x, nCases), matchTerm.tpe )
        else
           cpsScrutinee.monadFlatMap( x => Match(x, nCases), matchTerm.tpe )


object MatchTreeTransform:


  def run[F[_]:Type,T:Type](using qctx: QuoteContext)(cpsCtx: TransformationContext[F,T],
                         matchTerm: qctx.tasty.Match): CpsExpr[F,T] = {
                         
     val tmpFType = summon[Type[F]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
          
         def bridge(): CpsExpr[F,T] =
            val origin = matchTerm.asInstanceOf[qctx.tasty.Match]
            runMatch(origin).toResult(cpsCtx.patternCode).asInstanceOf[CpsExpr[F,T]]
                        

     } 
     (new Bridge(cpsCtx)).bridge()
  }

