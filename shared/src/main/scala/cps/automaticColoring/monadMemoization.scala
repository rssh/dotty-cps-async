package cps.automaticColoring

import cps._

import scala.quoted._

enum MonadMemoizationKind:
  case BY_DEFAULT, INPLACE, PURE

class ResolveMonadMemoizationKind[F[_]](val value: MonadMemoizationKind)

given resolveMemoizationKindFromExpr[F[_]:Type]: FromExpr[ResolveMonadMemoizationKind[F]] with 

  def unapply(x: Expr[ResolveMonadMemoizationKind[F]])(using Quotes): Option[ResolveMonadMemoizationKind[F]] =
    x match
       case '{ ResolveMonadMemoizationKind[F](${Expr(x)}) } =>
              Some(ResolveMonadMemoizationKind[F](x))
       case _ => None

given memoizationKindFromExpr: FromExpr[MonadMemoizationKind] with

  def unapply(x: Expr[MonadMemoizationKind])(using Quotes): Option[MonadMemoizationKind] =
    x match
       case '{ MonadMemoizationKind.BY_DEFAULT } => Some(MonadMemoizationKind.BY_DEFAULT)
       case '{ MonadMemoizationKind.INPLACE } => Some(MonadMemoizationKind.INPLACE)
       case '{ MonadMemoizationKind.PURE } => Some(MonadMemoizationKind.PURE)
       case _  => None



