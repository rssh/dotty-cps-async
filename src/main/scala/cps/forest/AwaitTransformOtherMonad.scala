package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._
 
object AwaitTransformOtherMonad:

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def apply[F[_]:Type,G[_]:Type, T:Type, S:Type](cpsCtx: TransformationContext[F,T], 
                                    gType:  Type[G],
                                    sType:  Type[S], 
                                    gs:Expr[Any])  // should be G[S], but dotty bug.
                                      ( using qctx: QuoteContext): CpsExpr[F,T] =
                                       
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     // TODO: think about situation, when ftType != tType.
     // [independend / translated ]
     val ftType = summon[quoted.Type[F]].unseal
     val gtType = gType.unseal
     if (gtType.tpe =:= ftType.tpe) {
        AwaitTransform.apply(cpsCtx,sType,gs.asInstanceOf[Expr[F[S]]])
     } else {
        qctx.warning("other monad: ${gType.show} vs ${fType.show}", patternCode)
        //TODO: call typedApply
        //CpsExprResult[F,T](patternCode, builder, patternType, true)
        throw MacroError("intersection of different asyc areas is not supported now", patternCode)
     }
     

