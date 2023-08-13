package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.*
import ast.tpd.*

import cps.plugin.*

object BlockTransform {

  def apply(term: Block, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"BlockTransform, term=${term.show}",nesting)
    val retval = term match
      case Block((ddef: DefDef)::Nil, closure: Closure)  if ddef.symbol == closure.meth.symbol =>
        // TODO:
        //   if we here, this is not an argument of function.
        Log.trace(s"BlockTransform: lambda",nesting)
        val cpsBody = {
          val ddefCtx = summon[Context].withOwner(ddef.symbol)
          val tctx = summon[CpsTopLevelContext]
          val ddefRhs = ddef.rhs(using ddefCtx)
          RootTransform(ddefRhs, ddef.symbol, nesting+1)(using ddefCtx, tctx)
        }
        LambdaCpsTree(term, owner, ddef, closure.tpe.widen, cpsBody)
      case Block(Nil, last) =>
        Log.trace(s"BlockTransform: empty block",nesting)
        val lastCps = RootTransform(last, owner,  nesting+1)
        val inBlock = lastCps.unpure match
          case None =>
            lastCps
          case Some(syncTree) => 
            if (syncTree eq last) then
              PureCpsTree(term,owner,term)
            else
              val t = Block(Nil, syncTree).withSpan(term.span)
              PureCpsTree(term,owner,t)
        BlockBoundsCpsTree(inBlock)  
      case Block(statements, last) =>
        val s0: CpsTree = CpsTree.unit(owner)
        val statsCps = statements.foldLeft(s0){ (s,e) =>
           e match
             case d: MemberDef =>
               d match
                 case v: ValDef =>
                   val cpsV: CpsTree = ValDefTransform(v, owner, nesting + 1)
                   Log.trace(s"adding valdef to block ${cpsV}",nesting)
                   s.appendInBlock(cpsV)
                 case mDef =>
                   // templates and local function definitions will be processed py compiler plugin in own,
                   //  we just will not skip local templates and local function definitions during select phase as internal.
                   val nTDef = MemberDefCpsTree(mDef, owner, mDef)
                   val r = s.appendInBlock(nTDef)
                   r
             case importTree: Import =>
               // ignore imports,
               //   (TODO:  is it correct?)  FirstTransform deleted all non-language imports, but what with language
               //   imports here ?
               s
             case _ =>
              val cpsE0 = RootTransform(e, owner, nesting+1)
              val cpsE = maybeApplyCustomDiscard(cpsE0, owner, nesting)
              val r = s.appendInBlock(cpsE)
              r
        }  
        val lastCps = RootTransform(last, owner, nesting+1)
        val blockCps = statsCps.appendInBlock(lastCps).withOrigin(term)
        BlockBoundsCpsTree(blockCps)
    Log.trace(s"BlockTransform, retval=${retval.show}",nesting)
    retval
  }

  // TODO: remove after elimination of automatic coloring
  def maybeApplyCustomDiscard(cpsTree:CpsTree, owner:Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
    val tctx = summon[CpsTopLevelContext]
    Log.trace(s"BlockTransform.maybeApplyCustomDiscard: customValueDiscard=${tctx.customValueDiscard}", nesting)
    if (!tctx.customValueDiscard) then
      cpsTree
    else
      if (cpsTree.originType != defn.UnitType && cpsTree.originType != defn.NothingType) then
        val valueDiscardType = Symbols.requiredClassRef("cps.ValueDiscard").appliedTo(cpsTree.originType.widen)
        CpsTransformHelper.findImplicitInstance(valueDiscardType, cpsTree.origin.span) match
          case Some(discard) =>
             applyImplicitDiscard(cpsTree, owner, discard)
          case None =>
              report.warning(s"custom discard is enablde, but no implicit instance for ${valueDiscardType.show} found", cpsTree.origin.srcPos)
              cpsTree
      else
        cpsTree

  }

  def applyImplicitDiscard(cpsTree:CpsTree, owner:Symbol, inDiscard:Tree)(using Context, CpsTopLevelContext): CpsTree = {
    import inlines.Inlines


    def genDiscardApply(discard: Tree, arg: Tree): Tree = {
      val discardApply = Select(discard, "apply".toTermName)
      val call = discardApply.appliedTo(arg)
      val retval = if (discardApply.symbol.flags.is(Flags.Inline)) then {
        Inlines.inlineCall(call)
      } else
        call
      retval
    }

    val discard = if (inDiscard.symbol.flags.is(Flags.Inline)) then
      Inlines.inlineCall(inDiscard)
    else
      inDiscard

    if (discard.tpe.baseType(Symbols.requiredClass("cps.AwaitValueDiscard")) != NoType) then
      if !(cpsTree.originType <:< summon[CpsTopLevelContext].monadType.appliedTo(Types.WildcardType)) then
        throw CpsTransformException(s"await discard is not applicable to ${cpsTree.originType.show}", cpsTree.origin.srcPos)
      cpsTree.unpure match
        case Some(stat) =>
          CpsTree.impure(cpsTree.origin, cpsTree.owner, stat, AsyncKind.Sync)
        case None =>
          cpsTree.asyncKind match
            case AsyncKind.Sync =>
              throw CpsTransformException(s"impossible: sync tree with empty unpure: ${cpsTree}", cpsTree.origin.srcPos)
            case AsyncKind.Async(internalKind) =>
              if (internalKind != AsyncKind.Sync) then
                throw CpsTransformException(s"impossible: async tree with non-sync internal kind: ${cpsTree}", cpsTree.origin.srcPos)
              val untpdTree = untpd.Apply(
                      untpd.Select(untpd.TypedSplice(summon[CpsTopLevelContext].cpsMonadRef), "flatten".toTermName),
                      List(untpd.TypedSplice(cpsTree.transformed))
              )
              val typedTree = ctx.typer.typed(untpdTree, summon[CpsTopLevelContext].monadType.appliedTo(Types.WildcardType))
              val fakeOrigin = Apply(Select(discard, "apply".toTermName),List(cpsTree.origin)  )
              CpsTree.impure(fakeOrigin, owner, typedTree, internalKind)
            case AsyncKind.AsyncLambda(bodyKind) =>
              throw CpsTransformException(s"discarede lambda expression: ${cpsTree}", cpsTree.origin.srcPos)
    else
      cpsTree.unpure match
        case Some(stat) =>
          val tree = genDiscardApply(discard, stat)
          val fakeOrigin = Typed(cpsTree.origin, TypeTree(defn.UnitType)).withSpan(cpsTree.origin.span)
          CpsTree.pure(fakeOrigin, cpsTree.owner, tree)
        case None =>
          cpsTree.asyncKind match
            case AsyncKind.Sync =>
              throw CpsTransformException(s"impossible: sync tree with empty unpure: ${cpsTree}", cpsTree.origin.srcPos)
            case AsyncKind.Async(ik) =>
              val toDiscardSym = Symbols.newSymbol(owner, "toDiscard".toTermName, Flags.Synthetic, cpsTree.originType)
              val toDiscardRef = ref(toDiscardSym)
              val toDiscardValDef = ValDef(toDiscardSym, EmptyTree)
              val discardBody = genDiscardApply(discard, toDiscardRef)
              val fakeOrigin0 = Apply(Select(discard, "apply".toTermName),List(cpsTree.origin)  ).withSpan(cpsTree.origin.span)
              val fakeOrigin1 = Apply(Select(discard, "apply".toTermName),List(toDiscardRef)  ).withSpan(cpsTree.origin.span)
              MapCpsTree(fakeOrigin0, owner, cpsTree,
                MapCpsTreeArgument(Some(toDiscardValDef), CpsTree.pure(fakeOrigin1 , owner, discardBody)))
            case AsyncKind.AsyncLambda(bodyKind) =>
              throw CpsTransformException(s"discarede lambda expression: ${cpsTree}", cpsTree.origin.srcPos)
  }

}