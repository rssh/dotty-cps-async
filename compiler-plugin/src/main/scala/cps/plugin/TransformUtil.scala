package cps.plugin

import dotty.tools.dotc.*
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import dotty.tools.dotc.util.SrcPos
import ast.tpd.*

case class TermWithIncorrectOwner(term: Tree, expectedOwner: Symbol, actualOwner: Symbol)

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
                        case Some(value) =>
                          if (value.span.exists) then value else value.withSpan(tree.span)
                        case None => tree
                  case tt: TypeTree =>
                     tt.tpe match
                        case tr: TermRef =>
                           map.get(tr.symbol) match
                              case Some(value) =>
                                 TypeTree(value.symbol.termRef).withSpan(tt.span)
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

   def methodTypeFromFunctionType(candidate: Type, srcPos: SrcPos)(using Context): Option[MethodType] = {
      candidate match
         case mt: MethodType =>
            Some(mt)
         case AppliedType(tycon, targs) if (defn.isFunctionSymbol(tycon.typeSymbol)) =>
            val paramNames = (1 to targs.length-1).map(i => ("arg"+i).toTermName).toList
            val paramTypes = targs.dropRight(1)
            val resultType = targs.last
            Some(MethodType(paramNames)(_ => paramTypes, _ => resultType))
         case _ =>
            if (candidate.baseType(defn.Function0).exists) then
               Some(MethodType(Nil)(x => Nil, x => candidate))
            else if (candidate.baseType(defn.Function1).exists) then
                val fun = candidate.baseType(defn.Function1)
                fun match
                   case AppliedType(tycon, targs) if defn.isFunctionSymbol(tycon.typeSymbol) =>
                      methodTypeFromFunctionType(fun, srcPos)
                   case _ =>
                      None
            else if (candidate.baseType(defn.Function2).exists) then
                val fun = candidate.baseType(defn.Function2)
                fun match
                   case AppliedType(tycon, targs) if defn.isFunctionSymbol(tycon.typeSymbol) =>
                      methodTypeFromFunctionType(fun, srcPos)
                   case _ =>
                      None
            else
               val maxArity = 22
               var found = false
               var i = 3
               var fun: Type = NoType
               while(!found && i < maxArity) {
                  if (candidate.baseType(defn.FunctionSymbol(i)).exists) then
                     val fun = candidate.baseType(defn.FunctionSymbol(i))
                     found = true
                  i = i + 1
               }
               if (found) then
                    fun match
                      case at: AnnotatedType => methodTypeFromFunctionType(at.parent, srcPos)
                      case rt: RefinedType => methodTypeFromFunctionType(rt.parent, srcPos)
                      case AppliedType(tycon, targs) if defn.isFunctionSymbol(tycon.typeSymbol) =>
                          methodTypeFromFunctionType(fun, srcPos)
                      case _ =>
                          throw CpsTransformException(s"internal error: $fun expected to be AppliedType", srcPos)
               else
                    None
   }

   def findSubtermWithOwner(tree:Tree, owner:Symbol)(using Context): Option[Tree] = {
      val finder = new TreeAccumulator[Option[Tree]] {
         def apply(x: Option[Tree], tree: Tree)(using Context): Option[Tree] =
            if (x.isDefined) then
               x
            else
               tree match
                 case xi: Ident if xi.symbol.maybeOwner.exists =>
                   if (xi.symbol == owner) then
                      println("found ident with direct owner")
                      Some(tree)
                   else {
                     findIndirectOwner(xi.symbol,owner,Nil) match
                        case Some(path) =>
                           println(s"found ident with indirect owner, path=${path}")
                           Some(tree)
                        case None =>
                           //println(s"owner of ${xi.show} is ${xi.symbol.owner.show} (${xi.symbol.owner.hashCode()})")
                           foldOver(x, tree)
                   }
                 case _ =>
                    if (tree.symbol.maybeOwner == owner) then
                       Some(tree)
                    else
                      findIndirectOwner(tree.symbol.maybeOwner, owner, Nil) match
                        case Some(path) =>
                            println(s"found tree with indirect old owner, path=${path.reverse.map(s=>s"${s}(${s.hashCode()})").mkString("->")}")
                            Some(tree)
                        case None =>
                            foldOver(x, tree)

         def  findIndirectOwner(x: Symbol, mbOwner:Symbol, path:List[Symbol]): Option[List[Symbol]] = {
           if (x == mbOwner) then
                Some(x::path)
           else if (!x.maybeOwner.exists) then
                None
           else
                findIndirectOwner(x.maybeOwner, mbOwner, x :: path)
         }

      }
      finder(None,tree)
   }

   def findSubtermWithOtherOwner(tree:Tree, owner: Symbol)(using Context): Option[Tree] = {
      val finder = new TreeAccumulator[Option[Tree]] {

         def checkOwner(x:Symbol, owner:Symbol)(using Context): Boolean = {
            if (x.owner == owner) {
               true
            } else if (x.owner.isWeakOwner) {
              checkOwner(x.owner, owner)
            } else if (owner.isWeakOwner) {
              checkOwner(x, owner.owner)
            } else {
               false
            }
         }

         def apply(x: Option[Tree], tree: Tree)(using Context): Option[Tree] =
            if (x.isDefined) then
               x
            else
               tree match
                 case xdef: MemberDef =>
                   if (!checkOwner(xdef.symbol,owner)) then
                      Some(tree)
                   else
                      x
                 case _ =>
                      foldOver(x, tree)
      }
      finder(None,tree)
   }

   def findSubtermsWithIncorrectOwner(tree:Tree, topOwner: Symbol)(using Context): List[Tree] = {
      val finder = new TreeAccumulator[List[Tree]] {

         def checkOwner(x:Symbol, owner:Symbol)(using Context): Boolean = {
            if (x.owner == owner) {
               true
            } else if (x.owner.isWeakOwner) {
              checkOwner(x.owner, owner)
            } else if (owner.isWeakOwner) {
              checkOwner(x, owner.owner)
            } else {
               false
            }
         }

         def apply(x: List[Tree], tree: Tree)(using Context): List[Tree] =
            if (x.nonEmpty) then
               x
            else
               tree match
                 case xdef: MemberDef =>
                   if (!checkOwner(xdef.symbol,summon[Context].owner)) then
                      tree :: x
                   else
                      xdef match
                        case xDefDef: DefDef =>
                            foldOver(x, xDefDef.rhs)(using summon[Context].withOwner(xDefDef.symbol))
                        case xValDef: ValDef =>
                            foldOver(x, xValDef.rhs)(using summon[Context].withOwner(xValDef.symbol))
                        case xTypeDef: TypeDef =>
                            foldOver(x, xTypeDef.rhs)(using summon[Context].withOwner(xTypeDef.symbol))
                        case other =>
                          throw CpsTransformException("MemberDef but not DefDef, ValDef or TypeDef", other.sourcePos)

                 case _ =>
                      foldOver(x, tree)
      }
      finder(Nil,tree)
   }

   final val COMMA = ","

}