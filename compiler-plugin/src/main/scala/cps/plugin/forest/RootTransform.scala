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
   * @param nesting Nesting level, for debug purposes
   * @return CpsTree with transformed term
   */
      def apply(term: Tree, owner:Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
        try
          val retval = term match
            case applyTerm: Apply => ApplyTransform(applyTerm, owner, nesting)
            case block: Block => BlockTransform(block, owner, nesting)
            case id: Ident => IdentTransform(id, owner, nesting)
            case il: Inlined => InlinedTransform(il, owner, nesting)
            case mt: Match => MatchTransform(mt, owner, nesting)
            case s: Select => SelectTransform(s, owner, nesting)
            case tIf@If(_,_,_) => IfTransform(tIf, owner, nesting)
            case t: TypeApply => TypeApplyTransform(t, owner, nesting)
            case vd:ValDef => ValDefTransform(vd, owner, nesting)
            case lt:Literal => LiteralTransform(lt, owner,  nesting)
            case tt:Typed => TypedTransform(tt, owner, nesting)
            case at:Assign => AssignTransform(at, owner, nesting)
            case nt:New => SyncTransform(nt, owner, nesting)
            case _ =>
              throw CpsTransformException(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)
          retval
        catch
          case ex: CpsTransformException =>
            Log.info(s":${term.show}", nesting)
            throw ex;
      }

}