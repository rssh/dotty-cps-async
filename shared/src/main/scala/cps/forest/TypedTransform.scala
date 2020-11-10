package cps.forest


import scala.quoted._

import cps._
import cps.misc._


class TypedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(t: qctx.reflect.Term, tp: qctx.reflect.TypeTree): CpsExpr[F,T] =
     import qctx.reflect._
     t.seal match 
       case '{ $t1:tt1 } =>
         val r = Async.nestTransform(t1, cpsCtx, TransformationContextMarker.Typed)
         if (!r.isAsync)  
            //if (!r.isChanged)
               CpsExpr.sync(monad, patternCode)
            //else
            //   ???
         else 
            r.map( '{ x => ${Typed('x.unseal, TypeTree.of[T]).asExprOf[T]} } )
       case _ =>
         throw MacroError("Can't determinate type for ${t}",t.seal) 


