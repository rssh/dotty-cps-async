package cps.plugin

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import ast.{Trees, tpd}
import core.*
import core.Decorators.*
import core.Constants.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import transform.Erasure
import cps.plugin
import util.SrcPos
import plugins.*
import cps.plugin.QuoteLikeAPI.*


/**
 * Generate adapoters fro the cps-transformed function be able to comply non-cpsed type in symbol denoatations,
 * because we can't change symbol denoatations before erasure phase.
 */
object Scaffolding {

  def adoptUncpsedRhs(tree: Tree, tType: Type,  fType:Type)(using Context): Tree = {
    val adoptSymbol = Symbols.requiredMethod("cps.plugin.scaffolding.adoptForUncpsedDenotation")
    val adoptIdent = ref(adoptSymbol)
    val adoptedTree = Apply(TypeApply(adoptIdent, List(TypeTree(fType), TypeTree(tType.widen))), tree :: Nil)
    adoptedTree
  }

  def adoptCpsedCall(tree: Tree, origType: Type, fType: Type)(using Context): Tree = {

    /**
     * Extract function from call application which can be carried.
     * @param call
     * @return
     */
    def extractCarriedFun(tree:Tree): Symbol = {
      val retval =tree match
        case Apply(TypeApply(Select(obj,applyCn),targs),args)
          if applyCn.toString == "apply" &&
             (defn.isFunctionNType(obj.tpe.widen) || defn.isContextFunctionType(obj.tpe.widen)) =>
            extractCarriedFun(obj)
        case TypeApply(Select(obj,applyCn),targs)
          if applyCn.toString == "apply" &&
            (defn.isFunctionNType(obj.tpe.widen) || defn.isContextFunctionType(obj.tpe.widen)) =>
            extractCarriedFun(obj)
        case Apply(Select(obj,applyCn),args)
          if applyCn.toString == "apply" &&
            (defn.isFunctionNType(obj.tpe.widen) || defn.isContextFunctionType(obj.tpe.widen)) =>
          extractCarriedFun(obj)
        case Select(obj,applyCn)
          if applyCn.toString == "apply" &&
            (defn.isFunctionNType(obj.tpe.widen) || defn.isContextFunctionType(obj.tpe.widen)) =>
          extractCarriedFun(obj)
        case Apply(TypeApply(internal, _), _) =>
          extractCarriedFun(internal)
        case Apply(fun, _) =>
          extractCarriedFun(fun)
        case TypeApply(fun, _) =>
          extractCarriedFun(fun)
        case Inlined(call, bindings, expansion) =>
          extractCarriedFun(expansion)
        case _ =>
          tree.symbol
      retval
    }

    val funSym = extractCarriedFun(tree)
    if (funSym != Symbols.NoSymbol) then
      if (!funSym.flags.is(Flags.Inline)  &&
          !funSym.hasAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsTransformed"))
      ) then
        report.error(s"looks like ${funSym.show} is compiled without dotty-cps-async plugin, tree: ${tree}", tree.srcPos)

    val adoptSymbol = Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")
    val adoptIdent = ref(adoptSymbol)
    Apply(TypeApply(adoptIdent, List(TypeTree(fType), TypeTree(origType))), tree :: Nil)
  }

  def isAdoptForUncpsedDenotation(sym: Symbol)(using Context): Boolean = {
      sym == Symbols.requiredMethod("cps.plugin.scaffolding.adoptForUncpsedDenotation")
  }

  def isAdoptCpsedCall(sym: Symbol)(using Context): Boolean = {
       sym == Symbols.requiredMethod("cps.plugin.scaffolding.adoptCpsedCall")
  }

  object Cpsed {

    def unapply(tree: Tree)(using Context): Option[Tree] = {
      tree match
        case Apply(fn, List(arg)) if (Scaffolding.isAdoptCpsedCall(fn.symbol)) =>
          // TODO:  (are we need check for isInstanceOf here?)
          val cpsedCall = arg match
            case Apply(fn1, List(arg1)) if Erasure.Boxing.isBox(fn1.symbol) =>
              arg1
            case Block(List(stat), expr) if expr == Literal(Constant(()))
              || expr.symbol.exists && expr.symbol == defn.BoxedUnit_UNIT =>
              stat
            case _ =>
              arg
          Some(cpsedCall)
        case _ => None
    }


  }


  object Uncpsed {

    def unapply(tree: Tree)(using Context): Option[Tree] = {
      tree match
        case TypeApply(sel@Select(internal, asInstanceOfCn), List(tpt))
          if (asInstanceOfCn.toString == "asInstanceOf") =>
          internal match
            case Uncpsed(internal1) =>
              Some(cpy.TypeApply(tree)(Select(internal1, asInstanceOfCn), List(TypeTree(internal1.tpe.widen))))
            case _ =>
              None
        case Apply(cnUnbox, List(internal)) if Erasure.Boxing.isUnbox(cnUnbox.symbol) =>
          internal match
            case Uncpsed(internal1) =>
              // TODO: set asInstanceOf ?
              Some(internal1)
            case _ => None
        case Block((ddef: DefDef) :: Nil, closure: Closure) =>
          ddef.rhs match
            case Uncpsed(nRhs) =>
              Some(cpy.Block(tree)(cpy.DefDef(ddef)(rhs = nRhs, tpt = TypeTree(nRhs.tpe.widen)) :: Nil, closure))
            case _ =>
              None
        case Apply(fn, List(arg)) if (Scaffolding.isAdoptForUncpsedDenotation(fn.symbol)) =>
          Some(arg)
        case Block(List(Uncpsed(internal)), Literal(Constant(()))) =>
          // we can have function Direct => Unit and it's can be wrapped in empty block to return Unit regardless of computation result.
          Some(internal)
        case _ => None
    }

  }


}

