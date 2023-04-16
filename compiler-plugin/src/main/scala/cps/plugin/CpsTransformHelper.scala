package cps.plugin

import scala.annotation.tailrec

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Symbols.*
import core.Types.*
import util.SrcPos
import util.Spans.Span


object CpsTransformHelper {

  //def cpsTransformClassSymbol(using Context) =
  //  Symbols.requiredClass("cps.E.CpsTransform")

  def cpsMonadContextClassSymbol(using Context) =
      Symbols.requiredClass("cps.CpsMonadContext")

  def isCpsMonadContextType(tpe:Type, debug:Boolean=false)(using Context): Boolean = {
     val retval = tpe.dealias match
       case AppliedType(tycon, List(targ)) if (tycon.typeSymbol == cpsMonadContextClassSymbol) =>
         true
       case other =>
         other.baseType(cpsMonadContextClassSymbol) != NoType
     if (debug)
        println(s"isCpsMonadContextType(${tpe.show} [tree:${tpe}]) = ${retval}")
     retval
  }

  /**
   *@param contextFunctionArgType is CpsMonadContext[F]
   *@return F
   **/
  def extractMonadType(contextFunctionArgType: Type, pos: SrcPos)(using Context): Type =
    contextFunctionArgType.dealias match
      case AppliedType(tycon, List(targ)) if (tycon.typeSymbol == cpsMonadContextClassSymbol) => 
             targ
      case other =>
             val cntBase = other.baseType(cpsMonadContextClassSymbol)
             if (cntBase != NoType) 
                cntBase match
                  case AppliedType(tycon, List(targ)) => targ
                  case _ =>
                    throw CpsTransformException(s"Can't extract monad from context-type: ${cntBase.show}", pos)
             else
                throw CpsTransformException("assument that contect function type is CpsMonadContext[T]", pos)


  /**
   * Transform method type of function (...params) ?=> T  to  (...params) ?=> Cps[F,T]
   * @param lambdaTree - origin function
   * @param params - list of parameters of lambda function
   * @param body - function boldy
   * @param fType - type of F[_]
   * @return params ?=> F[T]
   */           
  def transformContextualLambdaType(lambdaTree: Tree, params: List[ValDef], body: Tree, fType:Type)(using Context): MethodType = {
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpt.tpe)
    ContextualMethodType(paramNames)(
      x => paramTypes,
      x => cpsTransformedType(body.tpe.widen, fType)
    )    
  }


  /**
   * ([T1,...Tn,R]   tranfrom to [T1,...Tn,CpsType[F,R]]
   *@param targs - type arguments of origin context function,
   **/
  def adoptResultTypeParam(targs:List[Type],fType:Type)(using Context):List[Type] = {
    @tailrec
    def advance(rest:List[Type], acc:List[Type]):List[Type] = {
       rest match
         case Nil  => acc  // impossible
         case last::Nil  => (cpsTransformedType(last,fType) :: acc).reverse
         case head::tail => advance(tail, head::acc)
    }
    advance(targs,Nil)
  }

  /**
   * CpsType[F[_],T] =
   *   F[T]  is T is not function type
   *   (X1..Xn) => CpsType[F, Y]  if T = (X1...Xn) => Y
   **/
  def cpsTransformedType(t:Type, fType:Type, debug: Boolean = false)(using Context): Type = {
    val retval = t match
      case TermRef(prefix,name) =>
         t
      case AppliedType(funCn,params) =>
        if (debug) {
          println(s"cpsTransformedType, funCn = ${funCn.show}, isFunctionType=${defn.isFunctionType(funCn)}, isContextFunctionType=${defn.isContextFunctionType(funCn)}")
          println(s"cpsTransformedType, isContextFunctionType(t)=${defn.isContextFunctionType(t)}")
        }
        if (defn.isFunctionType(t) || defn.isContextFunctionType(t)) then
          val nParams = adoptResultTypeParam(params, fType)
          AppliedType(funCn, nParams)
        else
          decorateTypeApplications(fType).appliedTo(t)
      case mt: MethodType =>
        if (mt.isContextualMethod)
          mt.derivedLambdaType(mt.paramNames, mt.paramInfos, cpsTransformedType(mt.resType, fType, debug))
          //ContextualMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
        else if (mt.isImplicitMethod)
          ImplicitMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
        else
          MethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
      case pt: PolyType =>
        PolyType(pt.paramNames)(_ => pt.paramInfos, _ => cpsTransformedType(pt.resType, fType, debug))
      case _  =>
        decorateTypeApplications(fType).appliedTo(t)
    if (debug) {
      println(s"cpsTransformedType: in = (${t.show}   [${t}]")
      println(s"cpsTransformedType: out = ${retval.show}   [${retval}]")
    }
    retval
  }


  def findImplicitInstance(tpe: Type, span: Span)(using ctx:Context): Option[Tree] = {
    val searchResult = ctx.typer.inferImplicitArg(tpe,span)
    searchResult.tpe match
      case _ : typer.Implicits.SearchFailureType => None
      case _  => Some(searchResult)
  }
  
  def findRuntimeAwait(monadType: Type, span: Span)(using ctx:Context): Option[Tree] = {
      //TODO:  Problem: shows incorrect phase.
      val runtimeAwait = requiredClassRef("cps.CpsRuntimeAwait")
      val tpe = AppliedType(runtimeAwait, List(monadType))
      findImplicitInstance(tpe, span)
  }
  

}