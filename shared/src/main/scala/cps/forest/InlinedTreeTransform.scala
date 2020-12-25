// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait InlinedTreeTransform[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.reflect._

  
  def runInlined(origin: Inlined): CpsTree =
    val cpsBody = runRoot(origin.body, TransformationContextMarker.InlinedBody)
    val funValDefs = origin.bindings.filter{ x =>
       x match
          case vx@ValDef(name,tpt,Some(rhs)) =>
              checkLambdaDef(rhs) match
                 case Some(lambda) => true
                 case None => false
          case _ => false
    }
    if (cpsCtx.flags.debugLevel >= 15) then
        funValDefs.foreach{ b =>
           cpsCtx.log(s"fubValDef in binding: ${b.show}")
        } 
    if (origin.bindings.isEmpty) then
       cpsBody
    else
       // TODO: check lambda-s in bindings
       // TODO: inject await from bindings before CpsBody
       InlinedCpsTree(origin, cpsBody)


  def checkLambdaDef(term:Term):Option[Term] =
     term match
        case Block(List(),expr) => checkLambdaDef(expr)
        case lt@Lambda(params,body) => Some(lt)
        case _ => None


object InlinedTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         inlinedTerm: qctx1.reflect.Inlined): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = inlinedTerm.asInstanceOf[quotes.reflect.Inlined]
            runInlined(origin).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }



