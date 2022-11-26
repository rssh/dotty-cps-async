package cps.plugin.forest.application

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*
import cps.plugin.forest.*


trait MethodParamsDescriptor {

  def  paramIndex(name: String): Option[Int]

  def  paramName(index: Int): Option[String]

  def  paramType(index: Int): Option[TypeRepr]

  def  isByName(index: Int): Boolean =
        paramType(index) match
          case Some(ByNameType(_)) => true
          case _ => false

}


object MethodParamsDescriptor {

  def  apply(fun: Tree)(using Context): MethodParamsDescriptor =
    fun.tpe.widen.dealias match
      case mt: MethodType =>
                MethodTypeBasedParamsDescriptor(mt)
      case other =>
                throw CpsTransformException(s"apply to non-method, tpe=${fun.tpe}",fun.srcPos)
                
}


class MethodTypeBasedParamsDescriptor(mt: MethodType) extends MethodParamsDescriptor {

  override def  paramIndex(name: String): Option[Int] = paramIndexes.get(name)

  override def  paramName(index: Int): Option[String] =
       if (index >= 0 && index < paramNames.size)
         Some(paramNames(index))
       else
         None

  override def  paramType(index: Int): Option[TypeRepr] =
       if (index >= 0 && index < paramTypes.size)
         Some(paramTypes(index))
       else
         None

  private lazy val paramNames = mt.paramNames.toIndexedSeq
  private lazy val paramIndexes = paramNames.zipWithIndex.toMap
  private lazy val paramTypes = mt.paramTypes.toIndexedSeq


}
