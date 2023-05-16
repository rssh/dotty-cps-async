package cps.plugin.forest.application

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Names.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*
import dotty.tools.dotc.util.SrcPos


import cps.plugin.*
import cps.plugin.forest.*


trait MethodParamsDescriptor {

  def  paramIndex(name: String): Option[Int]

  def  paramName(index: Int, pos: SrcPos): TermName

  def  paramType(index: Int, pos: SrcPos): Type

  def  isByName(index: Int, pos: SrcPos): Boolean =
        paramType(index, pos) match
          case _: ExprType => true
          case _ => false

  def  isDirectContext(index: Int, pos: SrcPos)(using Context): Boolean

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

  override def  paramIndex(name: String): Option[Int] = paramIndexes.get(name.toTermName)

  override def  paramName(index: Int, pos: SrcPos): TermName =
       if (index >= 0 && index < paramNames.size)
         paramNames(index)
       else
         throw CpsTransformException(s"method $mt have no parameter with index $index", pos)

  override def  paramType(index: Int, pos: SrcPos): Type =
       if (index >= 0 && index < paramTypes.size)
         paramTypes(index)
       else
        throw CpsTransformException(s"method $mt have no parameter with index $index",pos)

  override def  isDirectContext(index: Int, pos: SrcPos)(using Context): Boolean =
    mt.isContextualMethod && CpsTransformHelper.isCpsDirectType(paramTypes(index))


  private lazy val paramNames = mt.paramNames.toIndexedSeq
  private lazy val paramIndexes = paramNames.zipWithIndex.toMap
  private lazy val paramTypes = mt.paramInfos.toIndexedSeq


}
