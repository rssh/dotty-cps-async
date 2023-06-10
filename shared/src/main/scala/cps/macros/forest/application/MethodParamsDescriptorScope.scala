package cps.macros.forest.application

import scala.quoted.*
import cps.*
import cps.macros.*
import cps.macros.forest.*
import cps.macros.misc.*

import scala.annotation.experimental


trait MethodParamsDescriptorScope[F[_], CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._



  trait MethodParamsDescriptor:

     def  paramIndex(name: String): Option[Int]

     def  paramName(index: Int): Option[String]

     def  paramType(index: Int): Option[TypeRepr]

     def  isContext: Boolean


     def  isCpsDirect(index:Int): Boolean =
            isContext && (
              paramType(index) match
                case Some(t) =>
                  t.derivesFrom(Symbol.requiredClass("cps.CpsDirect"))
                  //t <:< TypeRepr.of[CpsDirect[F]]
                case None => false
            )

     def  isByName(index: Int): Boolean =
           paramType(index) match
             case Some(ByNameType(_)) => true
             case _ => false


  object MethodParamsDescriptor:

     def  apply(fun: Term): MethodParamsDescriptor =
       fun.tpe.widen.dealias match
         case mt: MethodType =>
                   MethodTypeBasedParamsDescriptor(mt)
         case other =>
                   report.warning(s"apply to non-method, tpe=${fun.tpe}",posExpr(fun))
                   EmptyParamsDescriptor



  class MethodTypeBasedParamsDescriptor(mt: MethodType) extends MethodParamsDescriptor:

     override def  paramIndex(name: String): Option[Int] = paramIndexes.get(name)

     override def  paramName(index: Int): Option[String] =
       if (index >= 0 && index < paramNames.size)
         Some(paramNames(index))
       else
         None

     override def isContext: Boolean = {
       mt.isImplicit
     }

     override def  paramType(index: Int): Option[TypeRepr] =
       if (index >= 0 && index < paramTypes.size)
         Some(paramTypes(index))
       else
         None

     private lazy val paramNames = mt.paramNames.toIndexedSeq
     private lazy val paramIndexes = paramNames.zipWithIndex.toMap
     private lazy val paramTypes = mt.paramTypes.toIndexedSeq


  object EmptyParamsDescriptor extends MethodParamsDescriptor:

     override def  paramIndex(name: String): Option[Int] = None
     override def  paramName(index: Int): Option[String] = None
     override def  paramType(index: Int): Option[TypeRepr] = None
     override def  isContext: Boolean = false



  object DynaminParamsDescriptor extends MethodParamsDescriptor:

     override def  paramIndex(name: String): Option[Int] =
        scala.util.Try(name.toInt).toOption

     override def  paramName(index: Int): Option[String] =  Some(index.toString)
     override def  paramType(index: Int): Option[TypeRepr] = Some(TypeRepr.of[Any])
     override def  isContext: Boolean = false


