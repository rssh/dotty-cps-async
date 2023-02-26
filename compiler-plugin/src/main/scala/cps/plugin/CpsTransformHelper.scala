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

  def cpsTransformClassSymbol(using Context) =
    Symbols.requiredClass("cps.E.CpsTransform")

  /**
   *@param contextFunctionArgType is CpsTransform[F]
   *@return F
   **/
  def extractMonadType(contextFunctionArgType: Type, pos: SrcPos)(using Context): Type =
    contextFunctionArgType match
      case AppliedType(tycon, List(targ)) if (tycon.typeSymbol == cpsTransformClassSymbol) => 
             targ
      case _ =>
             throw CpsTransformException("assument that contect function type is CpsTransform[T]", pos)


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
  def cpsTransformedType(t:Type, fType:Type)(using Context): Type = {
    t match
      case AppliedType(funCn,params) if (defn.isFunctionType(funCn)) =>
        val nParams = adoptResultTypeParam(params, fType)
        AppliedType(funCn, nParams)
      case _  =>
        decorateTypeApplications(fType).appliedTo(t)
  }


  //  Problem here  --  we can't search implicit after typer.
  //  
  def findRuntimeAwait(monadType: Type, span: Span)(using ctx:Context): Option[Tree] = {
      val runtimeAwait = requiredClassRef("cps.RuntimeAwait")
      val tpe = AppliedType(runtimeAwait, List(monadType))
      val searchResult = ctx.typer.inferImplicitArg(tpe,span)
      searchResult.tpe match
        case _ : typer.Implicits.SearchFailureType => None
        case _  => Some(searchResult)
  }
  

}