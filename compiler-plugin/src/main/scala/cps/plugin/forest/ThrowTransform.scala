package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*


/**
 * Note, that actually throw is a special variant of Apply,  so
 * ThrowTransform is called from Apply,  not from RootTransform as others.
 */
object ThrowTransform {

  def apply(term:Apply, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    term.args match
      case List(arg) =>
        val cpsArg = RootTransform(arg,owner,nesting+1)
        cpsArg.asyncKind match
          case AsyncKind.Sync =>
            //Btw, for most of monads,  throw from monad a[[ly/lazyPure is handled well
            //  TODO: mb use this
            //if (cpsArg.isChanged)
            //  CpsTree.pure(origin, owner, genThrow(term, cpsArg.unpure.get))
            //else
            //  CpsTree.unchangedPure(origin,
            CpsTree.impure(term, owner, genMonadError(term, cpsArg.unpure.get), AsyncKind.Sync)
          case AsyncKind.Async(internalKind) =>
            val newSym = Symbols.newSymbol(owner, "xThrow".toTermName, Flags.EmptyFlags, cpsArg.originType.widen, coord = term.span)
            val nValDef = ValDef(newSym).withSpan(term.span)
            FlatMapCpsTree(term, owner, cpsArg,
              FlatMapCpsTreeArgument(Some(nValDef),
                       CpsTree.impure(term,owner,genMonadError(term,ref(newSym)), AsyncKind.Sync),
              )
            )
          case AsyncKind.AsyncLambda(bodyKind) =>
            throw CpsTransformException("Can't throw async lambda", term.srcPos)
      case _ =>
        throw CpsTransformException("throw must have exactly one argument", term.srcPos)
  }

  def genMonadError(origin: Apply, tree: Tree)(using Context, CpsTopLevelContext): Tree =
    val tctx = summon[CpsTopLevelContext]


    val throwMonad = {
      if (tctx.cpsMonadRef.tpe <:<  AppliedType(Symbols.requiredClassRef("cps.CpsThrowMonad"),List(WildcardType))) {
          tctx.cpsMonadRef
      } else if (tctx.cpsMonadContextRef.tpe <:< AppliedType(Symbols.requiredClassRef("cps.CpsDirect"),List(WildcardType))) {
          Select(tctx.cpsMonadContextRef, "throwMonad".toTermName)
      } else if (tctx.cpsMonadContextRef.tpe <:< AppliedType(Symbols.requiredClassRef("cps.CpsThrowMonadContext"),List(WildcardType))) {
          Select(tctx.cpsMonadContextRef, "monad".toTermName)
      } else {
        // TODO: lool at throw support ?
        throw CpsTransformException(s"throw is not supported for such monad ${tctx.cpsMonadRef.tpe.widen.show}", origin.srcPos)
      }
    }

    Apply(
      TypeApply(
        Select(throwMonad, "error".toTermName),
        List(TypeTree(origin.tpe.widen))
      ),
      List(tree)
    ).withSpan(origin.span)


}
