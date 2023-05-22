package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*


import cps.plugin.*


sealed trait ShapedCpsTree {
  def cpsTree: CpsTree
}

case object ShapedCpsTree {

  trait Sync(unpure: Tree) extends ShapedCpsTree with Product {
    def _1: Tree = unpure
  }

  object Sync {
    def unapply(s: ShapedCpsTree): Option[Sync] =
      if (s.isInstanceOf[Sync])
        Some(s.asInstanceOf[Sync])
      else None
  }

  trait Async(internalKind: AsyncKind) extends ShapedCpsTree with Product {
    def _1: AsyncKind = internalKind
  }

  object Async {
    def unapply(s: ShapedCpsTree): Option[Async] =
      if (s.isInstanceOf[Async])
        Some(s.asInstanceOf[Async])
      else None
  }

  // todo: add params
  trait AsyncLambda(bodyLind: AsyncKind, paramss: List[Type], originResultType: Type) extends ShapedCpsTree with Product {
    def _1: AsyncKind = bodyLind
    def _2: List[Type] = paramss
    def _3: Type = originResultType
  }

  object AsyncLambda {
    def unapply(s: ShapedCpsTree): Option[AsyncLambda] =
      if (s.isInstanceOf[AsyncLambda])
        Some(s.asInstanceOf[AsyncLambda])
      else None
  }

}

