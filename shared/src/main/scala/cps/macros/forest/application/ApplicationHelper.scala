package cps.macros.forest.application

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.forest._
import cps.macros.misc._


trait ApplicationHelper[F[_],CT] extends ApplyArgRecordScope[F,CT] 
                                      with MethodParamsDescriptorScope[F,CT]
                                      with ApplyArgBuilderScope[F,CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>


