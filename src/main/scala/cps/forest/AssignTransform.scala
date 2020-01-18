package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class AssignTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Assign(left,right) 
  def run(given qctx: QuoteContext)(left: qctx.tasty.Term, right: qctx.tasty.Term): CpsExprResult[F,T] = 
     import qctx.tasty.{_, given}
     println(s"!!! assign detected : ${left} ${right}")
     left.seal match 
        case '{ $le: $lt } =>
            val cpsLeft = Async.rootTransform(le,asyncMonad,false)
            // shpuld have to structure in such waym as workarround against
            //  
            runWithLeft(left,right,cpsLeft)
        case _ =>
            throw MacroError("Can't determinate type",left.seal)


  def runWithLeft[L:Type](given qctx: QuoteContext)(
       left: qctx.tasty.Term, right: qctx.tasty.Term, cpsLeft:CpsExprResult[F,L]): CpsExprResult[F,T] = {
     import qctx.tasty.{_, given}
     right.seal match {
        case '{ $re: $rt } =>
            val cpsRight = Async.rootTransform(re,asyncMonad,false)
            run1(left,right,cpsLeft,cpsRight)
        case _ =>
            throw MacroError("Can't determinate type",right.seal)
     }
  }

  //implicit def getOrigin[S](x:CpsExprResult[F,S]): quoted.Type[S] = x.originType


  def run1[L:Type,R:Type](given qctx: QuoteContext)(left: qctx.tasty.Term, right: qctx.tasty.Term,
                cpsLeft: CpsExprResult[F,L], cpsRight: CpsExprResult[F,R]): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     if (!cpsLeft.isAsync) {
        val builder =  if (!cpsRight.isAsync) 
                          CpsChunkBuilder.sync(asyncMonad, patternCode)
                       else    // !cpsLeft.isAsync && cpsRight.isAsync
                          CpsChunkBuilder.async(asyncMonad,
                               cpsRight.chunkBuilder.map[T]( 
                                 '{ (x:R) => ${Assign(left,'x.unseal).seal.asInstanceOf[Expr[T]] } 
                                  }).toExpr  )
        CpsExprResult(patternCode, builder, patternType)
     } else { // (cpsLeft.isAsync) {
        left match 
          case Select(obj,sym) => 
              obj.seal match 
                 case '{ $o: $ot } =>
                    val lu = Async.rootTransform(o,asyncMonad,false)
                    run2(left,right,cpsLeft,cpsRight,lu)
                 case _ =>
                    throw MacroError("Can't determinate type",obj.seal)
          case _ =>  // non-assignable entity ?
              throw MacroError("assign to async non-select is impossible",patternCode)
     }


  def run2[L:Type,R:Type,LU:Type](given qctx: QuoteContext)(
            left: qctx.tasty.Term, right: qctx.tasty.Term,
             cpsLeft: CpsExprResult[F,L], cpsRight: CpsExprResult[F,R],
             cpsLu: CpsExprResult[F,LU]): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     val builder = if (!cpsRight.isAsync) {
                           CpsChunkBuilder.async[F,T](asyncMonad,
                                 cpsLu.chunkBuilder.map[T]('{ x => 
                                        ${Assign('x.unseal.select(left.symbol), right).seal.
                                                  asInstanceOf[Expr[T]] } }).toExpr
                           )
                   } else {
                           CpsChunkBuilder.async[F,T](asyncMonad,
                                 cpsLu.chunkBuilder.flatMap[T]('{ l =>
                                     ${cpsRight.chunkBuilder.flatMap[T]( 
                                        '{ r => ${
                                               Assign('l.unseal.select(left.symbol),
                                                      'r.unseal
                                               ).seal.asInstanceOf[Expr[F[T]]]
                                         }}
                                      ).toExpr }
                                 }).toExpr
                           )
                   }
     CpsExprResult(patternCode, builder, patternType)


