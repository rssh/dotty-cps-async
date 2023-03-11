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


      def apply(term: Tree, owner: Symbol, tctx: TransformationContext, nesting: Int)(using Context): CpsTree = {
        val retval = term match
          case applyTerm: Apply => ApplyTransform(applyTerm, owner, tctx, nesting)
          case block: Block => BlockTransform(block, owner, tctx, nesting)
          case id: Ident => IdentTransform(id, owner, tctx, nesting)
          case s: Select => SelectTransform(s, owner, tctx, nesting)
          case tIf@If(_,_,_) => IfTransform(tIf, owner, tctx, nesting)
          case t: TypeApply => TypeApplyTransform(t,owner,tctx, nesting)
          case vd:ValDef => ValDefTransform(vd,owner,tctx, nesting)
          case lt:Literal => LiteralTransform(lt,owner,tctx, nesting)
          case _ =>
            throw CpsTransformException(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)
        println(s"rootTransform: origin=${term.show}")
        println(s"rootTransform: result=${retval.show}")
        println(s"rootTransform: result.transformed=${retval.transformed.show }")
        retval
      }

}