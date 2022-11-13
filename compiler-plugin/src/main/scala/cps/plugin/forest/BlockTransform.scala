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

  def apply(term: Block, owner: Symbol, tctx: TransformationContext)(using Context): CpsTree = {
    term match
      case Block((ddef: DefDef)::Nil, closure: Closure)  =>
        // TODO:
        //   if we here, this is not an argument of function. 
        val cpsBody = rootTransform(ddef.rhs, tctx)
        LambdaCpsTree(tctx, term, owner, ddef, cpsBody)
      case Block(statements, last) =>
        val statsCps = statements.foldLeft(CpsTree.unit(owner)){ (s,e) =>
           val cpsE = rootTransform(e)
           s.append(cpsE)
        }  
        val lastCps = statsCps.append(last)
        lastCps.withOrigin(term)
  }

}