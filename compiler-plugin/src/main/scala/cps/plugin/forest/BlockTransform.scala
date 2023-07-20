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
        LambdaCpsTree(term, owner, ddef, cpsBody)
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
              val cpsE = RootTransform(e, owner, nesting+1)
              Log.trace(s"BlockTransform::appendInBlock, before s=${s.show} cpsE=${cpsE}",nesting)
              val r = s.appendInBlock(cpsE)
              Log.trace(s"BlockTransform::appendInBlock, after s=${r.show}",nesting)
              r
        }  
        val lastCps = RootTransform(last, owner, nesting+1)
        val blockCps = statsCps.appendInBlock(lastCps).withOrigin(term)
        BlockBoundsCpsTree(blockCps)
    Log.trace(s"BlockTransform, retval=${retval.show}",nesting)
    retval
  }

}