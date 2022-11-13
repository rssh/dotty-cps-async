package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Symbols.*
import ast.tpd.*
import plugins.*

object QuoteLikeAPI:

  object CheckLambda {

    /**
    *  add old symbol
    **/
    def unapply(tree: Tree)(using Context): Option[(List[ValDef],Tree,Symbol)] =
      tree match
        //case Block((ddef @ DefDef(_,ValDefs(params)::Nil,_,Some(body)))::Nil, closure: Closure) 
        case Block((ddef: DefDef)::Nil, closure: Closure) 
              if ddef.symbol == closure.meth.symbol =>
                ddef.paramss match
                  case ValDefs(params)::Nil =>
                             Some((params,ddef.rhs,ddef.symbol))
                  case _ => None
        case _ => None

  }

  
  


end QuoteLikeAPI