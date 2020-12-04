package cps.forest.application

import scala.quoted._

import cps._
import cps.forest._
import cps.misc._


trait ApplicationHelper[F[_],CT] extends ApplyArgRecordScope[F,CT] 
                                      with MethodParamsDescriptorScope[F,CT]
                                      with ApplyArgBuilderScope[F,CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>


