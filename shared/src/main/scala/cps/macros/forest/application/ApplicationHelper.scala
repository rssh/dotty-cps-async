package cps.macros.forest.application

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.forest._
import cps.macros.misc._


trait ApplicationHelper[F[_],CT, CC<:CpsMonadContext[F]] extends ApplyArgRecordScope[F,CT, CC] 
                                      with MethodParamsDescriptorScope[F,CT, CC]
                                      with ApplyArgBuilderScope[F,CT, CC]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>


