package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait AwaitTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F, CT] =>
  
  import qctx.tasty.{_, given _}


  def runAwait(awaitTerm: Term, args: Term): CpsTree =
      AwaitCpsTree(args, awaitTerm.tpe) 
      


