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

  def apply(term: Block, ctx: TransformationContext)(using Context): CpsTree = {
    term match
      case Block((ddef: DefDef)::Nil, closure: Closure)  =>
        // TODO:
        //   if we here, this is not an argument of function. 
        throw CpsTransformException(s"NotImplemented: lambda: $term",term.srcPos)
      case _ =>  
        throw CpsTransformException(s"NotImplemented: block : $term",term.srcPos)
  }

}