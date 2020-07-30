// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class InlinedTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Inlined(call, binding, expansion)
  def run(using qctx: QuoteContext)(inlinedTerm: qctx.tasty.Inlined): CpsExpr[F,T] =
    val bodyExpr = inlinedTerm.body.seal.asInstanceOf[Expr[T]]
    val nested = Async.rootTransform(bodyExpr,monad,flags,exprMarker,nesting+1)
    if (inlinedTerm.bindings.isEmpty)
      nested
    else
      InlinedCpsExpr(using qctx)(monad, Seq(), inlinedTerm, nested)


class InlinedCpsExpr[F[_]:Type,T:Type](using qctx0: QuoteContext)(
                     monad: Expr[CpsMonad[F]],
                     prev: Seq[ExprTreeGen],
                     oldInlined: qctx0.tasty.Inlined,
                     nested: CpsExpr[F,T]) extends CpsExpr[F, T](monad, prev) {

   override def isAsync = nested.isAsync

   override def fLast(using qctx: QuoteContext): Expr[F[T]] =
      import qctx.tasty._
      val qctxOldInlined = oldInlined.asInstanceOf[qctx.tasty.Inlined]
      val t = Inlined.copy(qctxOldInlined)(qctxOldInlined.call,
                               qctxOldInlined.bindings,
                               nested.transformed.unseal)
      t.seal.asInstanceOf[Expr[F[T]]]

   override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
      new InlinedCpsExpr(using qctx0)(monad, exprs ++: prev, oldInlined, nested)

   override def append[A:Type](chunk: CpsExpr[F,A])(using QuoteContext): CpsExpr[F,A] =
      if (nested.isAsync)
         InlinedCpsExpr(using qctx0)(monad, prev, oldInlined, nested.append(chunk))
      else
         chunk.prependExprs(Seq(StatementExprTreeGen(using qctx0)(oldInlined)))

   def syncOrigin(using QuoteContext): Option[Expr[T]] =
      if (nested.isAsync)
        None
      else
        val expr = oldInlined.seal.asInstanceOf[Expr[T]]
        Some(expr)

   override def map[A:Type](f: Expr[T => A])(using QuoteContext): CpsExpr[F,A] =
      syncOrigin match
         case None => MappedCpsExpr(monad,Seq(),this,f)
         case Some(origin) => CpsExpr.sync(monad, Expr.betaReduce('{ $f($origin) }))

}

