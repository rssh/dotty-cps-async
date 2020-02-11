package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_]]:

  thisTreeTransform: TreeTransformScope[F] =>
  
  import qctx.tasty.{_, given}


  def runAwait(awaitTerm: Term, args: Term): CpsTree =
      AwaitCpsTree(args, awaitTerm.tpe) 
      


