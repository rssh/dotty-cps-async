package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class AssignTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Assign(left,right) 
  def run(using qctx: QuoteContext)(left: qctx.reflect.Term, right: qctx.reflect.Term): CpsExpr[F,T] = 
     import qctx.reflect._
     left.seal match 
        case '{ $le: lt } =>
            val cpsLeft = Async.nestTransform(le,cpsCtx,TransformationContextMarker.AssignLeft)
            // shpuld have to structure in such waym as workarround against
            //  
            runWithLeft(left,right,cpsLeft)
        case _ =>
            throw MacroError("Can't determinate type",left.seal)


  def runWithLeft[L:Type](using qctx: QuoteContext)(
       left: qctx.reflect.Term, right: qctx.reflect.Term, cpsLeft:CpsExpr[F,L]): CpsExpr[F,T] = {
     import qctx.reflect._
     right.seal match {
        case '{ $re: rt } =>
            val cpsRight = Async.nestTransform(re,cpsCtx,TransformationContextMarker.AssignRight)
            run1(left,right,cpsLeft,cpsRight)
        case _ =>
            throw MacroError("Can't determinate type",right.seal)
     }
  }

  //implicit def getOrigin[S](x:CpsExprResult[F,S]): quoted.Type[S] = x.originType


  def run1[L:Type,R:Type](using qctx: QuoteContext)(left: qctx.reflect.Term, right: qctx.reflect.Term,
                cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R]): CpsExpr[F,T] =
     import qctx.reflect._
     if (!cpsLeft.isAsync) {
        if (!cpsRight.isAsync) 
            CpsExpr.sync(monad, patternCode)
        else    // !cpsLeft.isAsync && cpsRight.isAsync
            CpsExpr.async(monad,
                   cpsRight.map[T]( 
                         '{ (x:R) => ${Assign(left,'x.unseal).asExprOf[T] } 
                          }).transformed )
     } else { // (cpsLeft.isAsync) {
        left match 
          case Select(obj,sym) => 
              obj.seal match 
                 case '{ $o: ot } =>
                    val lu = Async.nestTransform(o,cpsCtx,TransformationContextMarker.AssignSelect)
                    run2(left,right,cpsLeft,cpsRight,lu)
                 case _ =>
                    throw MacroError("Can't determinate type",obj.seal)
          case _ =>  // non-assignable entity ?
              throw MacroError("assign to async non-select is impossible",patternCode)
     }


  def run2[L:Type,R:Type,LU:Type](using qctx: QuoteContext)(
            left: qctx.reflect.Term, right: qctx.reflect.Term,
             cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R],
             cpsLu: CpsExpr[F,LU]): CpsExpr[F,T] =
     import qctx.reflect._
     if (!cpsRight.isAsync) {
          CpsExpr.async[F,T](monad,
               cpsLu.map[T]('{ x => 
                    ${Assign('x.unseal.select(left.symbol), right).asExprOf[T]
                                                                       } }).transformed
         )
     } else {
         CpsExpr.async[F,T](monad,
               cpsLu.flatMap[T]('{ l =>
                                     ${cpsRight.flatMap[T]( 
                                        '{ r => ${
                                               Assign('l.unseal.select(left.symbol),
                                                      'r.unseal
                                               ).asExprOf[F[T]]
                                         }}
                                      ).transformed  }
                                 }).transformed
                           )
     }


