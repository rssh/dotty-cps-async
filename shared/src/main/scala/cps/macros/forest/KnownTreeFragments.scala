package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


trait KnownTreeFragments[F[_], CT, CC <: CpsMonadContext[F]]:

  thisKnownTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._


  lazy val awaitSymbol = Symbol.requiredMethod("cps.await")

  lazy val monadTypeTree = Inferred(TypeRepr.of[F])

  lazy val pureSymbol = Select.unique(cpsCtx.monad.asTerm,"pure").symbol

  lazy val mapSymbol = Select.unique(cpsCtx.monad.asTerm,"map").symbol

  lazy val flatMapSymbol = Select.unique(cpsCtx.monad.asTerm,"flatMap").symbol


  lazy val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe

  lazy val partialFunctionType = TypeIdent(Symbol.classSymbol("scala.PartialFunction")).tpe

