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


      def apply(term: Tree, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
        val retval = term match
          case applyTerm: Apply => ApplyTransform(applyTerm, owner, tctx)
          case block: Block => BlockTransform(block, owner, tctx)
          case id: Ident => IdentTransform(id, owner, tctx)
          case s: Select => SelectTransform(s, owner, tctx)
          case tIf@If(_,_,_) => IfTransform(tIf, owner, tctx)
          case t: TypeApply => TypeApplyTransform(t,owner,tctx)
          case vd:ValDef => ValDefTransform(vd,owner,tctx)
          case lt:Literal => LiteralTransform(lt,owner,tctx)
          case _ =>
            throw CpsTransformException(s"Unsupported tree in cps: $term",term.srcPos)
            //report.error(s"Unsupported tree in cps: $term",term.srcPos)
        println(s"rootTransform: origin=${term.show}")
        println(s"rootTransform: result=${retval.show}")
        println(s"rootTransform: result.transformed=${retval.transformed.show }")
        retval
      }

}