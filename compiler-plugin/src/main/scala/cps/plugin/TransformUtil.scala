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

}