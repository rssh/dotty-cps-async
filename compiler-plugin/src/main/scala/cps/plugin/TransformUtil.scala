package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*


object TransformUtil {

   def substParams(term: Tree, input:List[ValDef], output:List[Tree])(using Context): Tree = {
      val map = (input zip output).foldLeft(Map.empty[Symbol,Tree]){ (s,e) =>
         val (vd, t) = e
         s.updated(vd.symbol, t)
      }
      substParamsMap(term,map)
   }
  

   def substParamsMap(term: Tree, map:Map[Symbol,Tree])(using Context): Tree = {
      if (map.isEmpty) then
         term
      else
         val transformer = new TreeMap {
            override def transform(tree:Tree)(using Context) = {
               tree match
                  case tree: Ident if (tree.isTerm) =>
                     map.get(tree.symbol) match
                        case Some(value) => value
                        case None => tree
                  case _ =>
                     super.transform(tree)
            }
         }
         transformer.transform(term)
   }

   def makeLambda(params: List[ValDef], resultType: Type, owner: Symbol,  body: Tree, bodyOwner: Symbol)(using Context): Block = {
      val paramNames = params.map(_.name)
      val paramTypes = params.map(_.tpe.widen)
      val mt = MethodType(paramNames)(
         x => paramTypes,
         x => resultType
      )    
      val meth = Symbols.newAnonFun(owner,mt)
      val retval = Closure(meth,tss => {
             TransformUtil.substParams(body,params,tss.head).changeOwner(bodyOwner,meth)
         }
      )
      retval
   }

   final val COMMA = ","

}