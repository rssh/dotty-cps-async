package cps.plugin.forest

import cps.plugin.AsyncKind.Async
import cps.plugin.{AsyncKind, CpsTopLevelContext, CpsTransformException, TransformUtil}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.StdNames.*

object NonLocalReturnsThrowReturnTransform {

  def apply(origin:Apply, owner: Symbol, nesting: Int, method: Tree, targs2:List[Tree], arg2:Tree, returner:Tree)(using Context, CpsTopLevelContext): CpsTree = {
    val shiftedNonLocalReturnsThrowReturn = Symbols.requiredMethod("cps.runtime.util.control.NonLocalReturnsAsyncShift.throwReturn")

    val refThrowReturn = ref(shiftedNonLocalReturnsThrowReturn)

    def buildCall(arg:Tree): Tree = {
      Apply(
        Apply(
          TypeApply(
            ref(shiftedNonLocalReturnsThrowReturn).withSpan(origin.fun.span),
            targs2
          ),
          List(arg)
        ).withSpan(origin.fun.span),
        List(returner)
      ).withSpan(origin.span)
    }

    val cpsArg = RootTransform(arg2,owner,nesting+1)

    cpsArg.asyncKind match
      case AsyncKind.Sync =>
        CpsTree.pure(origin,owner,buildCall(cpsArg.unpure.get))
      case AsyncKind.Async(nested) =>
        CpsTree.impure(origin,owner, buildCall(cpsArg.unpure.get), nested)
      case AsyncKind.AsyncLambda(bodyKind) =>
        cpsArg.unpure match
          case Some(arg) =>
            // actually this is functional value.
            CpsTree.pure(origin,owner,buildCall(arg))
          case None =>
            throw CpsTransformException(s"Async lambda can't be used as argument of throwReturn", origin)
  }

}
