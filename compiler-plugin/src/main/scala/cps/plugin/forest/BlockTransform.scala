package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object BlockTransform {

  def apply(term: Block, owner: Symbol, tctx: TransformationContext, nesting: Int)(using Context): CpsTree = {
    term match
      case Block((ddef: DefDef)::Nil, closure: Closure)  =>
        // TODO:
        //   if we here, this is not an argument of function.
        val cpsBody = {
          val ddefCtx = summon[Context].withOwner(ddef.symbol)
          RootTransform(ddef.rhs(using ddefCtx), ddef.symbol, tctx, nesting+1)(using ddefCtx)
        }
        LambdaCpsTree(tctx, term, owner, ddef, cpsBody)
      case Block(Nil, last) =>
        val lastCps = RootTransform(last, owner, tctx, nesting+1)
        val inBlock = lastCps.unpure match
          case None =>
            lastCps
          case Some(syncTree) => 
            if (syncTree eq last) then
              PureCpsTree(tctx,term,owner,term)
            else
              val t = Block(Nil, syncTree).withSpan(term.span)
              PureCpsTree(tctx,term,owner,t)
        BlockBoundsCpsTree(inBlock)  
      case Block(statements, last) =>
        val s0: CpsTree = CpsTree.unit(tctx, owner)
        val statsCps = statements.foldLeft(s0){ (s,e) =>
           val cpsE = RootTransform(e, owner, tctx, nesting+1)
           s.appendInBlock(cpsE)
        }  
        val lastCps = RootTransform(last,owner,tctx, nesting+1)
        val blockCps = statsCps.appendInBlock(lastCps).withOrigin(term)
        BlockBoundsCpsTree(blockCps)
  }

}