package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Types.*
import ast.tpd.*



case class TransformationContext(
  val monadType: Type,  // F[_]
  val cpsMonadRef: Tree  // summon[CpsTryMonad[F]]
)
