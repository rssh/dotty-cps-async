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
     if (cpsCtx.flags.debugLevel > 10)
        println(s"!!!Block-transform, prevs = $prevs")
        println(s"!!!Block-transform, last = $last")
     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
        if (cpsCtx.flags.debugLevel > 10)
           println(s"!!!Block-transform, i=$i, p=$p")
        p match
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                ValDefTransform.fromBlock(using qctx)(cpsCtx.copy(exprMarker=exprMarker+i.toString), v)
              case _ =>
                DefCpsExpr(using qctx)(cpsCtx.asyncMonad,Seq(),d)
            } 
          case t: Term =>
            // TODO: rootTransform
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
  

class DefCpsExpr[F[_]:Type](using qctx: QuoteContext)(
                     monad: Expr[AsyncMonad[F]],
                     prev: Seq[ExprTreeGen],
                     definition: qctx.tasty.Definition) extends SyncCpsExpr[F, Unit](monad, prev) {

  def last(using QuoteContext): Expr[Unit] = '{ () }

  def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,Unit] =
       if (exprs.isEmpty) 
         this
       else
         new DefCpsExpr(using qctx)(monad,exprs ++: prev,definition)

  def append[A:Type](chunk: CpsExpr[F,A])(using qctx: QuoteContext): CpsExpr[F,A] =
       chunk.prependExprs(Seq(StatementExprTreeGen(using this.qctx)(definition))).prependExprs(prev)


}
