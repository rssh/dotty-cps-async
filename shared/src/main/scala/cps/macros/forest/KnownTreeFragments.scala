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


  lazy val nonLocalReturnsSym = Symbol.classSymbol("scala.util.control.NonLocalReturns$")

  lazy val nonLocalReturnsReturningSym = nonLocalReturnsSym.declaredMethod("returning").head

  lazy val nonLocalReturnsThrowReturnSym = nonLocalReturnsSym.declaredMethod("throwReturn").head

  lazy val shiftedNonLocalReturnsThrowReturnSym = Symbol.classSymbol("cps.runtime.util.control.NonLocalReturnsAsyncShift$").declaredMethod("throwReturn").head

  lazy val shiftedNonLocalReturnsReturningSym = Symbol.classSymbol("cps.runtime.util.control.NonLocalReturnsAsyncShift$").declaredMethod("returning").head

  lazy val shiftedNonLocalReturnsSyncReturningSym = Symbol.classSymbol("cps.runtime.util.control.NonLocalReturnsAsyncShift$").declaredMethod("syncReturning").head

  lazy val nonFatalUnapplySym = Symbol.classSymbol("scala.util.control.NonFatal$").declaredMethod("unapply").head

  lazy val nonFatalAndNotControlThrowableAsyncWrapperClassSym = Symbol.classSymbol("cps.runtime.util.control.NonFatalAndNotControlThrowableAsyncWrapper$")
   
  lazy val nonFatalAndNotControlThrowableAsyncWrapperCompanion = Ref.term(nonFatalAndNotControlThrowableAsyncWrapperClassSym.companionModule.termRef)


  lazy val logicalAndSym = defn.BooleanClass.declaredMethod("&&").head
  lazy val logicalOrSym = defn.BooleanClass.declaredMethod("||").head
  lazy val logicalNotSym = defn.BooleanClass.declaredMethod("unary_!").head

  lazy val cpsNotChangeSymbol = Symbol.classSymbol("cps.plugin.annotation.CpsNotChange")
  lazy val adoptCpsedCallSymbol = Symbol.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")



