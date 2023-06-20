package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Constants.*
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.StdNames.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span
import core.Types.*
import core.Phases.*

import cps.plugin.*


object AsynchronizedTransform {


  def fromApply(term: Apply, owner: Symbol, nesting: Int, directType: Tree, valueType: Tree, internalTerm:Tree, directRef:Tree)(using Context, CpsTopLevelContext): CpsTree = {
    internalTerm match
      case Block((ddef: DefDef)::Nil, closure: Closure) =>
        parseLambda(term, owner, nesting, directType, valueType, ddef, closure, directRef)
      case Block(Nil,Block((ddef: DefDef)::Nil, closure: Closure)) =>
        parseLambda(term, owner, nesting, directType, valueType, ddef, closure, directRef)
      case _ =>
        throw CpsTransformException("lambda expected", internalTerm.srcPos)
  }

  def parseLambda(term: Apply, owner: Symbol, nesting: Int, monadTypeTree: Tree, valueTypeTree: Tree, ddef: DefDef, closure: Closure, directRef: Tree)(using Context, CpsTopLevelContext): CpsTree = {
    val tctx = summon[CpsTopLevelContext]
    val cpsRhs = RootTransform(ddef.rhs, owner, nesting+1)
    val context = Select(directRef,"context".toTermName)
    val monad = Select(context,"monad".toTermName)
    val internalContextType = Select(monad,"Context".toTypeName).tpe
    val paramValDefs = ddef.paramss.head.head match
      case v: ValDef => List(v)
      case _ =>
        throw CpsTransformException("Expected that lambda as parameter of asynchoronized have no type parameters", ddef.srcPos)
    val mt = MethodType(List("asynchronizedCtx".toTermName))(
      _ => List(internalContextType),
      _ => cpsRhs.transformedType.widen
    )
    val meth = Symbols.newAnonFun(owner, mt)
    val nLambda = Closure(meth, tss => {
      val directType = directRef.tpe.widen
      val directSym = Symbols.newSymbol(meth, "direct".toTermName, Flags.EmptyFlags, directType)
      val nDirect = ValDef(directSym,New(directType,tss.head)).withSpan(term.span)
      Block(
        List(nDirect),
        TransformUtil.substParams(cpsRhs.transformed, paramValDefs, List(ref(directSym))).changeOwner(ddef.symbol, meth)
      )
    })
    val nTree = Apply(TypeApply(Select(monad,"apply".toTermName),List(valueTypeTree)),List(nLambda)).withSpan(term.span)
    val retval = CpsTree.pure(term,owner,nTree)
    Log.trace(s"!AsynchronizedTransform.fromApply: retval = ${retval.show},  isOriginEqSync=${retval.isOriginEqSync}", nesting)
    retval
  }

}
