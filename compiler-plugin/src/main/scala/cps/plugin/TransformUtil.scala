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
                  case tt: TypeTree =>
                     tt.tpe match
                        case tr: TermRef =>
                           map.get(tr.symbol) match
                              case Some(value) =>
                                 TypeTree(value.symbol.termRef)
                              case None =>
                                 super.transform(tt)
                        case _ =>
                           super.transform(tt)
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

   def realWiden(tpe:Type)(using Context): Type =
      tpe match
         case tpeOr@OrType(tp1,tp2) =>
            val tp1w = realWiden(tp1)
            val tp2w = realWiden(tp2)
            if (tp1w eq tp1) && (tp2w eq tp2) then
               tpe
            else
               OrType.make(tp1w,tp2w,tpeOr.isSoft)
         case _ => tpe.widen

   def collectDefOwners(tree:Tree)(using Context): List[(Symbol,Symbol)] = {
      val gather = new TreeAccumulator[List[(Symbol,Symbol)]] {
         def apply(x: List[(Symbol,Symbol)], tree: Tree)(using Context): List[(Symbol,Symbol)] =
            tree match
               case tree: DefTree =>
                  ((tree.symbol,tree.symbol.owner)) :: x
               case _ =>
                  foldOver(x, tree)
      }
      gather(Nil,tree)
   }

   final val COMMA = ","

}