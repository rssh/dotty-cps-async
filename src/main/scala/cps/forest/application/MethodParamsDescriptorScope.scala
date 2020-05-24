package cps.forest.application

import scala.quoted._

import cps._
import cps.forest._
import cps.misc._


trait MethodParamsDescriptorScope[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.tasty._

  trait MethodParamsDescriptor:

     def  paramIndex(name: String): Option[Int]

     def  paramName(index: Int): Option[String]

     def  paramType(index: Int): Option[Type]



