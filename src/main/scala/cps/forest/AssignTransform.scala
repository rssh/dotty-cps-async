package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class AssignTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Assign(left,right)
  def run(using qctx: QuoteContext)(left: qctx.tasty.Term, right: qctx.tasty.Term): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     left.asExpr match
        case '{ $le: $lt } =>
            val cpsLeft = Async.nestTransform(le,cpsCtx,TransformationContextMarker.AssignLeft)
            // shpuld have to structure in such waym as workarround against
            //
            runWithLeft(left,right,cpsLeft)
        case _ =>
            throw MacroError("Can't determinate type",left.asExpr)


  def runWithLeft[L:Type](using qctx: QuoteContext)(
       left: qctx.tasty.Term, right: qctx.tasty.Term, cpsLeft:CpsExpr[F,L]): CpsExpr[F,T] = {
     import qctx.tasty.{_, given _}
     right.asExpr match {
        case '{ $re: $rt } =>
            val cpsRight = Async.nestTransform(re,cpsCtx,TransformationContextMarker.AssignRight)
            run1(left,right,cpsLeft,cpsRight)
        case _ =>
            throw MacroError("Can't determinate type",right.asExpr)
     }
  }

  //implicit def getOrigin[S](x:CpsExprResult[F,S]): quoted.Type[S] = x.originType


  def run1[L:Type,R:Type](using qctx: QuoteContext)(left: qctx.tasty.Term, right: qctx.tasty.Term,
                cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R]): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     if (!cpsLeft.isAsync) {
        if (!cpsRight.isAsync)
            CpsExpr.sync(monad, patternCode)
        else    // !cpsLeft.isAsync && cpsRight.isAsync
            CpsExpr.async(monad,
                   cpsRight.map[T](
                         '{ (x:R) => ${Assign(left,'x.asTerm).asExprOf[T] }
                          }).transformed )
     } else { // (cpsLeft.isAsync) {
        left match
          case Select(obj,sym) =>
              obj.asExpr match
                 case '{ $o: $ot } =>
                    val lu = Async.nestTransform(o,cpsCtx,TransformationContextMarker.AssignSelect)
                    run2(left,right,cpsLeft,cpsRight,lu)
                 case _ =>
                    throw MacroError("Can't determinate type",obj.asExpr)
          case _ =>  // non-assignable entity ?
              throw MacroError("assign to async non-select is impossible",patternCode)
     }


  def run2[L:Type,R:Type,LU:Type](using qctx: QuoteContext)(
            left: qctx.tasty.Term, right: qctx.tasty.Term,
             cpsLeft: CpsExpr[F,L], cpsRight: CpsExpr[F,R],
             cpsLu: CpsExpr[F,LU]): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     if (!cpsRight.isAsync) {
          CpsExpr.async[F,T](monad,
               cpsLu.map[T]('{ x =>
                    ${Assign('x.asTerm.select(left.symbol), right).asExpr.
                                          asInstanceOf[Expr[T]] } }).transformed
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


