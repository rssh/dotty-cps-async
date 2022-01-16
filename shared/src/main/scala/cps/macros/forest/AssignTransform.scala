package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


class AssignTransform[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._

  // case Assign(left,right)
  def run(using Quotes)(left: quotes.reflect.Term, right: quotes.reflect.Term): CpsExpr[F,T] =
     import quotes.reflect._
     left.asExpr match
        case '{ $le: lt } =>
            val cpsLeft = Async.nestTransform(le,cpsCtx)
            // shpuld have to structure in such waym as workarround against
            //
            runWithLeft(left,right,cpsLeft)
        case _ =>
            throw MacroError("Can't determinate type",left.asExpr)


  def runWithLeft[L:Type](using Quotes)(
       left: quotes.reflect.Term, right: quotes.reflect.Term, cpsLeft:CpsExpr[F,L]): CpsExpr[F,T] = {
     import quotes.reflect._
     right.asExpr match {
        case '{ $re: rt } =>
            val cpsRight = Async.nestTransform(re,cpsCtx)
            run1(left,right,cpsLeft,cpsRight)
        case _ =>
            throw MacroError("Can't determinate type",right.asExpr)
     }
  }


  def run1[L:Type,R:Type](using Quotes)(left: quotes.reflect.Term, right: quotes.reflect.Term,
                cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R]): CpsExpr[F,T] =
     import quotes.reflect._
     if (!cpsLeft.isAsync) {
        if (!cpsRight.isAsync) 
            CpsExpr.sync(monad, patternCode, false)
        else    // !cpsLeft.isAsync && cpsRight.isAsync
            CpsExpr.async(monad,
                   cpsRight.map[T]( 
                         '{ (x:R) => ${Assign(left,'x.asTerm).asExprOf[T] } 
                          }).transformed )
     } else { // (cpsLeft.isAsync) {
        left match
          case Select(obj,sym) =>
              obj.asExpr match
                 case '{ $o: ot } =>
                    val lu = Async.nestTransform(o,cpsCtx)
                    run2(left,right,cpsLeft,cpsRight,lu)
                 case _ =>
                    throw MacroError("Can't determinate type",obj.asExpr)
          case _ =>  // non-assignable entity ?
              throw MacroError("assign to async non-select is impossible",patternCode)
     }


  def run2[L:Type,R:Type,LU:Type](using Quotes)(
            left: quotes.reflect.Term, right: quotes.reflect.Term,
             cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R],
             cpsLu: CpsExpr[F,LU]): CpsExpr[F,T] =
     import quotes.reflect._
     if (!cpsRight.isAsync) {
          CpsExpr.async[F,T](monad,
               cpsLu.map[T]('{ x => 
                    ${Assign('x.asTerm.select(left.symbol), right).asExprOf[T]
                                                                       } }).transformed
         )
     } else {
         CpsExpr.async[F,T](monad,
               cpsLu.flatMap[T]('{ l =>
                                     ${cpsRight.flatMap[T](
                                        '{ r => ${
                                               Assign('l.asTerm.select(left.symbol),
                                                      'r.asTerm
                                               ).asExprOf[F[T]]
                                         }}
                                      ).transformed  }
                                 }).transformed
                           )
     }


