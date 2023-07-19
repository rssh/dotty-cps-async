package cps.plugin.forest

import cps.plugin.{AsyncKind, CpsTopLevelContext, CpsTransformException, TransformUtil}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*


object NonLocalReturnsTransform {


  /**
   * @param term Applhy contains NonLocalReturns.returning(...) call.
   * @param owner -  owner of call.
   * @param nesting - nesting level of cps processing
   * @param targ - type of returning value
   * @param arg - returning value
   * @param Context
   * @param CpsTopLevelContext

   * @return
   */
  def apply(term: Apply, owner:Symbol, nesting: Int, targ: Tree, arg: Tree)(using Context, CpsTopLevelContext): CpsTree = {

    val nonFatalUnapplySym = Symbols.requiredClass("scala.util.control.NonFatal$").requiredMethod("unapply")
    val nonFatalAndNotControlThrowableAsyncWrapperObj = ref(Symbols.requiredModule("cps.runtime.util.control.NonFatalAndNotControlThrowableAsyncWrapper"))
    val nonLocalReturnsAsyncShift = Symbols.requiredModule("cps.runtime.util.control.NonLocalReturnsAsyncShift")
    val nonLocalReturns = Symbols.requiredModule("scala.util.control.NonLocalReturns")


    val substituteNonFatal = new TreeMap {
      override def transform(tree: Tree)(using Context): Tree = {
        tree match
          case u: UnApply if u.fun.symbol == nonFatalUnapplySym =>
            val nFun = Select(nonFatalAndNotControlThrowableAsyncWrapperObj, "unapply".toTermName)
            cpy.UnApply(u)(nFun, u.implicits, u.patterns)
          //for scala-3.3.2 - add QuotePAtter  => ???
          case _ =>
            super.transform(tree)
      }
    }

    arg match
      //case Inlined(call, bindings, expansion) =>
      case Block((ddef:DefDef)::Nil, closure:Closure) if (closure.meth.symbol == ddef.symbol) =>
         val cpsRhs = RootTransform(ddef.rhs, ddef.symbol, nesting+1)
         cpsRhs.unpure match
           case Some(syncRhs) =>
             if (cpsRhs.isOriginEqSync) {
               CpsTree.unchangedPure(term,owner)
             } else {
               val lambdaParams = ddef.paramss.head.asInstanceOf[List[ValDef]]
               val nLambda = TransformUtil.makeLambda(lambdaParams, cpsRhs.originType.widen, owner, syncRhs, ddef.symbol)
               val nApply = Apply(term.fun, List(nLambda)).withSpan(term.span)
               CpsTree.pure(term,owner,nApply)
             }
           case None =>
             val lambdaParams = ddef.paramss.head.asInstanceOf[List[ValDef]]
             val nBody = substituteNonFatal.transform(cpsRhs.transformed)
             val nLambda = TransformUtil.makeLambda(lambdaParams, cpsRhs.transformedType.widen, owner, nBody, ddef.symbol).withSpan(arg.span)
             val nTerm = Apply(
               Apply(
                 TypeApply(
                   Select(ref(nonLocalReturnsAsyncShift), "returning".toTermName ),
                   TypeTree(summon[CpsTopLevelContext].monadType) :: targ::Nil
                 ),
                 List(ref(nonLocalReturns), summon[CpsTopLevelContext].cpsMonadRef )
               ).withSpan(term.fun.span),
               List(nLambda)
             ).withSpan(term.span)
             CpsTree.impure(term,owner,nTerm,AsyncKind.Sync)
      case _ =>
        throw CpsTransformException(s"Unexpected tree as argument of NonLocalReturns.returning, should be lambda, we have ${arg}",arg.srcPos)
  }


}
