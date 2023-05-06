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

  def apply(term: Block, oldOwner: Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    term match
      case Block((ddef: DefDef)::Nil, closure: Closure)  if ddef.symbol == closure.meth.symbol =>
        // TODO:
        //   if we here, this is not an argument of function.
        val cpsBody = {
          val ddefCtx = summon[Context].withOwner(ddef.symbol)
          val tctx = summon[CpsTopLevelContext]
          RootTransform(ddef.rhs(using ddefCtx), ddef.symbol, ddef.symbol, nesting+1)(using ddefCtx, tctx)
        }
        LambdaCpsTree(term, newOwner, ddef, cpsBody)
      case Block(Nil, last) =>
        val lastCps = RootTransform(last, oldOwner, newOwner, nesting+1)
        val inBlock = lastCps.unpure match
          case None =>
            lastCps
          case Some(syncTree) => 
            if (syncTree eq last) then
              val nTerm = term.changeOwner(oldOwner, newOwner)
              PureCpsTree(nTerm,newOwner,nTerm)
            else
              val t = Block(Nil, syncTree).withSpan(term.span)
              PureCpsTree(term,newOwner,t)
        BlockBoundsCpsTree(inBlock)  
      case Block(statements, last) =>
        val s0: CpsTree = CpsTree.unit(newOwner)
        val statsCps = statements.foldLeft(s0){ (s,e) =>
           val cpsE = RootTransform(e, oldOwner, newOwner, nesting+1)
           s.appendInBlock(cpsE)
        }  
        val lastCps = RootTransform(last, oldOwner, newOwner, nesting+1)
        val blockCps = statsCps.appendInBlock(lastCps).withOrigin(term)
        BlockBoundsCpsTree(blockCps)
  }

}