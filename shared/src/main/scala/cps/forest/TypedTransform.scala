package cps.forest


import scala.quoted._

import cps._
import cps.misc._


class TypedTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  def run(using qctx: QuoteContext)(t: qctx.tasty.Term, tp: qctx.tasty.TypeTree): CpsExpr[F,T] =
     import qctx.reflect._
     t.seal match 
       case '{ $t1:$T1 } =>
         val r = Async.nestTransform(t1, cpsCtx, TransformationContextMarker.Typed)
         if (!r.isAsync)  
            //if (!r.isChanged)
               CpsExpr.sync(monad, patternCode)
            //else
            //   ???
         else 
            val tType = summon[quoted.Type[T]]
            r.map( '{ x => ${Typed('x.unseal, tType.unseal).seal.cast[T]} } )
       case _ =>
         throw MacroError("Can't determinate type for ${t}",t.seal) 


