// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Block(prevs,last) 
  def run(using qctx: QuoteContext)(prevs: List[qctx.tasty.Statement], last: qctx.tasty.Term): CpsExpr[F,T] =
     val tType = implicitly[Type[T]]
     import qctx.tasty.{_, given _}
     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
        p match
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                ValDefTransform.fromBlock(using qctx)(cpsCtx.copy(exprMarker=exprMarker+i.toString), v)
              case _ =>
                printf(d.show)
                throw MacroError("definition is not supported inside block",patternCode)
            } 
          case t: Term =>
            t.seal match 
                case '{ $p:$tp } =>
                        Async.nestTransform(p, cpsCtx, i.toString)
                case other =>
                        printf(other.show)
                        throw MacroError(s"can't handle term in block: $other",t.seal)
          case i:Import =>
                   ImportTransform.fromBlock(using qctx)(cpsCtx.copy(exprMarker=exprMarker+"i"),i)
          case other =>
                printf(other.show)
                throw MacroError(s"unknown tree type in block: $other",patternCode)
     }
     val rLast = Async.nestTransform(last.seal.asInstanceOf[Expr[T]],cpsCtx,"B")
     val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
     // wrap yet in one Expr, to 'seal' (not unroll during append in enclosing block).
     CpsExpr.wrap(blockResult)
  


