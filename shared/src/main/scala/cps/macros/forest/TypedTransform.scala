package cps.macros.forest


import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


class TypedTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._

  def run(using Quotes)(t: quotes.reflect.Term, tp: quotes.reflect.TypeTree): CpsExpr[F,T] =
     import quotes.reflect._
     t.asExpr match
       case '{ $t1:tt1 } =>
         val r = Async.nestTransform(t1, cpsCtx)
         if (!r.isAsync)  
            if (!r.isChanged)
               CpsExpr.sync(monad, patternCode, false)
            else
               val expr = Typed(r.syncOrigin.get.asTerm, tp).asExprOf[T]
               CpsExpr.sync(monad, expr, true)
         else 
            r.map( '{ x => ${Typed('x.asTerm, TypeTree.of[T]).asExprOf[T]} } )
       case _ =>
         throw MacroError("Can't determinate type for ${t}",t.asExpr)


