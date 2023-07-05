package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*


object DefDefTransform {

  case class TransformResult(tree: DefDef, tctx: CpsTopLevelContext)

  def apply(ddef: DefDef, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val optKind = SelectedNodes.detectDefDefSelectKind(ddef)
    optKind match
      case None => MemberDefCpsTree(ddef, owner, ddef)
      case Some(kind) =>
        val transformResult = transform(ddef, kind, Some(summon[CpsTopLevelContext]))
        MemberDefCpsTree(ddef,owner,transformResult.tree)
  }

  def transform(tree: DefDef, selectKind: DefDefSelectKind, optTopLevelContext: Option[CpsTopLevelContext] = None)(using Context): TransformResult = {
    val debugSettings = optTopLevelContext.map(_.settings).getOrElse(DebugSettings.make(tree))
    val debugLevel = debugSettings.debugLevel
    println(s"transformDefDef ${tree.symbol.showFullName}, (${tree.symbol.id}) starting at${tree.srcPos.startPos.show}, selectKind=${selectKind}")
    if (debugSettings.printCode) then
      report.log("transforming tree:", tree.srcPos)
    val retval = selectKind match
      case DefDefSelectKind.USING_CONTEXT_PARAM(cpsMonadContextArg) =>
        val cpsMonadContext = ref(cpsMonadContextArg.symbol)
        val tc = makeCpsTopLevelContext(cpsMonadContext, tree.symbol, tree.srcPos, debugSettings, CpsTransformHelper.cpsDirectClassSymbol)
        val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
        if (! optTopLevelContext.forall(_.monadType =:= tc.monadType) ) {
          throw CpsTransformException(s"monad type mismatch between ${tree.symbol.fullName} and top-level: ${optTopLevelContext.map(_.monadType)} != ${tc.monadType}", tree.srcPos)
        }
        //selectRecord.changedReturnType = nTpt
        given CpsTopLevelContext = tc

        val ctx1: Context = summon[Context].withOwner(tree.symbol)
        val transformedRhs = RootTransform(tree.rhs, tree.symbol, 0)(using ctx1, tc).transformed
        val nRhs = Block(tc.cpsMonadValDef :: Nil, transformedRhs)(using ctx1)
        println(s"nRsh.block=${nRhs.show}")
        println(s"nRhs.tpe = ${nRhs.tpe.show}")
        val adoptedRhs = Scaffolding.adoptUncpsedRhs(nRhs, tree.tpt.tpe, tc.monadType)
        val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, adoptedRhs)
        println(s"transformDefDef[1] for ${tree.symbol.id}  ")
        //tree.symbol.defTree = retval
        TransformResult(retval, tc.monadType)
      case DefDefSelectKind.RETURN_CONTEXT_FUN(internalKind) =>
        tree.rhs match
          case oldLambda@Block((ddef: DefDef) :: Nil, closure: Closure) =>
            val nDefDef = transform(ddef, internalKind, optTopLevelContext)
            val cpsDirectContext = ref(selectRecord.kind.getCpsDirectContext.symbol)
            val fType = CpsTransformHelper.extractMonadType(cpsDirectContext.tpe, CpsTransformHelper.cpsDirectClassSymbol, tree.srcPos)
            //selectRecord.monadType = fType
            val nClosureType = CpsTransformHelper.cpsTransformedType(closure.tpe, fType)
            //selectRecord.changedReturnType = nClosureType
            val nClosure = Closure(closure.env, ref(nDefDef.symbol), EmptyTree).withSpan(closure.span)
            val nLambda = Block(nDefDef :: Nil, nClosure).withSpan(oldLambda.span)
            val retval = cpy.DefDef(tree)(tree.name, tree.paramss, tree.tpt, nLambda)
            TransformedResult(retval, fType)
          case _ =>
            throw CpsTransformException("Lambda function was expected, we have $tree", tree.srcPos)
    if (debugSettings.printCode) then
      report.log(s"transforned: ${retval.show}", tree.srcPos)
      report.log(s"transforned: ${retval}", tree.srcPos)
    retval
  }


}
