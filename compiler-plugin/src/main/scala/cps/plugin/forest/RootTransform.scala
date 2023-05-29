package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import core.Names.*
import ast.tpd.*

import cps.plugin.*

object RootTransform {

  /**
   *
   * @param term  Term to transform
   * @param owner  Current owner (usually - owner of current context).  If you need change owner, do this before calling transform.
   * @param targetType if set, target type - unwrapped type,  which is expected by typer.
   * @param nesting Nesting level, for debug purposes
   * @return CpsTree with transformed term
   */
      def apply(term: Tree, owner:Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
        try
          val retval = term match
            case applyTerm: Apply    => ApplyTransform(applyTerm, owner, nesting)
            case block: Block        => BlockTransform(block, owner, nesting)
            case ident: Ident        => IdentTransform(ident, owner, nesting)
            case inlineTerm: Inlined => InlinedTransform(inlineTerm, owner,  nesting)
            case matchTerm: Match    => MatchTransform(matchTerm, owner,  nesting)
            case selectTerm: Select  => SelectTransform(selectTerm, owner,  nesting)
            case ifTerm@If(_,_,_)    => IfTransform(ifTerm, owner, nesting)
            case typeApply: TypeApply => TypeApplyTransform(typeApply, owner, nesting)
            case valDef:ValDef        => ValDefTransform(valDef, owner, nesting)
            case literal:Literal      => LiteralTransform(literal, owner,  nesting)
            case typedTerm:Typed      => TypedTransform(typedTerm, owner,  nesting)
            case tryTerm:Try          => TryTransform(tryTerm, owner,  nesting)
            case assignTerm:Assign    => AssignTransform(assignTerm, owner,  nesting)
            case newTerm:New          => SyncTransform(newTerm, owner,  nesting)
            case superTerm:Super      => SyncTransform(superTerm, owner,  nesting)
            case thisTerm:This        => SyncTransform(thisTerm, owner,  nesting)
            case whileTerm: WhileDo   => WhileTransform(whileTerm, owner, nesting)
            case returnTerm:Return    =>
              throw CpsTransformException(s"Return is not supported, use NonLocalReturns instead", returnTerm.srcPos)
            case _  =>
              throw CpsTransformException(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)
          retval
        catch
          case ex: CpsTransformException =>
            Log.info(s":${term.show}", nesting)
            throw ex;
      }

}