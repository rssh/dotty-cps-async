package cps.plugin

import scala.annotation.tailrec

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import util.SrcPos


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
   * Transform method type of function (...params) ?=> T  to  (...params) ?=> F[T]
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
      x => decorateTypeApplications(fType).appliedTo(body.tpe.widen)
    )    
  }


  /**
   * ([T1,...Tn,R], ([T1,..Tn] ?=> F[R])  tranfrom to [T1,...Tn,F[R]]
   *@param targs - type arguments of origin context function,
   **/
  def adoptResultTypeParam(targs:List[Type],mt:MethodType) = {
    @tailrec
    def advance(rest:List[Type], acc:List[Type]):List[Type] = {
       rest match
         case Nil  => acc  // impossible
         case last::Nil  => (mt.resType :: acc).reverse
         case head::tail => advance(tail, head::acc)
    }
    advance(targs,Nil)
  }

}