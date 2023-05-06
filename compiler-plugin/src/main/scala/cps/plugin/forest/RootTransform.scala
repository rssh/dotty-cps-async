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


      def apply(term: Tree, oldOwner:Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
        try
          val retval = term match
            case applyTerm: Apply => ApplyTransform(applyTerm, oldOwner, newOwner, nesting)
            case block: Block => BlockTransform(block, oldOwner, newOwner, nesting)
            case id: Ident => IdentTransform(id, oldOwner, newOwner, nesting)
            case il: Inlined => InlinedTransform(il, oldOwner, newOwner, nesting)
            case s: Select => SelectTransform(s, oldOwner, newOwner, nesting)
            case tIf@If(_,_,_) => IfTransform(tIf, oldOwner, newOwner, nesting)
            case t: TypeApply => TypeApplyTransform(t, oldOwner, newOwner, nesting)
            case vd:ValDef => ValDefTransform(vd, oldOwner, newOwner, nesting)
            case lt:Literal => LiteralTransform(lt, oldOwner, newOwner,  nesting)
            case tt:Typed => TypedTransform(tt, oldOwner, newOwner, nesting)
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