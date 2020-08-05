// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Block(prevs,last)
  def run(using qctx: QuoteContext)(prevs: List[qctx.tasty.Statement], last: qctx.tasty.Term): CpsExpr[F,T] =
     if (cpsCtx.flags.debugLevel >= 10) then
        cpsCtx.log(s"Block transform, last=${last.show}")
     val tType = implicitly[Type[T]]
     import qctx.tasty.{_, given _}
     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
        p match
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                ValDefTransform.fromBlock(using qctx)(cpsCtx.copy(exprMarker=exprMarker+i.toString), v)
              case _ =>
                DefCpsExpr(using qctx)(cpsCtx.monad,Seq(),d)
            }
          case t: Term =>
            // TODO: rootTransform
            t.asExpr match
                case '{ $p:$tp } =>
                        Async.nestTransform(p, cpsCtx, i.toString)
                case other =>
                        printf(other.show)
                        throw MacroError(s"can't handle term in block: $other",t.asExpr)
          case i:Import =>
                   ImportTransform.fromBlock(using qctx)(cpsCtx.copy(exprMarker=exprMarker+"i"),i)
          case other =>
                printf(other.show)
                throw MacroError(s"unknown tree type in block: $other",patternCode)
     }
     val rLast = Async.nestTransform(last.asExprOf[T],cpsCtx,"B")
     val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
     // wrap yet in one Expr, to avoid unrolling during append in enclosing block).
     val retval = CpsExpr.wrap(blockResult)
     if (cpsCtx.flags.debugLevel >= 15) then
         cpsCtx.log(s"last.isAsync=${rLast.isAsync}")
         cpsCtx.log(s"blockResult.isAsync=${blockResult.isAsync}")
         cpsCtx.log(s"wrapped.isAsync=${retval.isAsync}")
         if (blockResult.isAsync==false && retval.isAsync==true) {
              cpsCtx.log(s"blockResult=${blockResult}")
              cpsCtx.log(s"blockResult.syncOrigin=${blockResult.syncOrigin}")
         }

     retval


class DefCpsExpr[F[_]:Type](using qctx: QuoteContext)(
                     monad: Expr[CpsMonad[F]],
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
