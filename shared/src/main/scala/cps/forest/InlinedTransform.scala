// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class InlinedTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):


  // case Inlined(call, binding, expansion)
  def run(using Quotes)(inlinedTerm: quotes.reflect.Inlined): CpsExpr[F,T] =
    val bodyExpr = inlinedTerm.body.asExprOf[T]
    val nested = Async.nestTransform(bodyExpr, cpsCtx, TransformationContextMarker.InlinedBody)
    if (inlinedTerm.bindings.isEmpty)
      nested
    else
      InlinedCpsExpr(using quotes)(cpsCtx.monad, Seq(), inlinedTerm, nested)


class InlinedCpsExpr[F[_]:Type,T:Type](using qctx0: Quotes)(
                     monad: Expr[CpsMonad[F]],
                     prev: Seq[ExprTreeGen],
                     oldInlined: qctx0.reflect.Inlined,
                     nested: CpsExpr[F,T]) extends CpsExpr[F, T](monad, prev) {

   override def isAsync = nested.isAsync

   override def fLast(using Quotes): Expr[F[T]] =
      import quotes.reflect._
      val qctxOldInlined = oldInlined.asInstanceOf[quotes.reflect.Inlined]
      val t = Inlined.copy(qctxOldInlined)(qctxOldInlined.call,
                               qctxOldInlined.bindings,
                               nested.transformed.asTerm)
      t.asExprOf[F[T]]

   override def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,T] =
      new InlinedCpsExpr(using qctx0)(monad, exprs ++: prev, oldInlined, nested)

   override def append[A:Type](chunk: CpsExpr[F,A])(using Quotes): CpsExpr[F,A] =
      if (nested.isAsync)
         InlinedCpsExpr(using qctx0)(monad, prev, oldInlined, nested.append(chunk))
      else
         chunk.prependExprs(Seq(StatementExprTreeGen(using qctx0)(oldInlined)))

   def syncOrigin(using Quotes): Option[Expr[T]] =
      if (nested.isAsync)
        None
      else
        val expr = oldInlined.asExprOf[T]
        Some(expr)

   override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[F,A] =
      syncOrigin match
         case None => MappedCpsExpr(monad,Seq(),this,f)
         case Some(origin) => CpsExpr.sync(monad, Expr.betaReduce('{ $f($origin) }))

}

