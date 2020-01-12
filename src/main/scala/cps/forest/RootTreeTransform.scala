package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait RootTreeTransform[F[_]]

  thisTransform: TreeTransformScope[F] =>
  
  import qctx.tasty.{_, given}

  def runRoot(term: qctx.tasty.Term): CpsTree =
     term.tpe.widen match {
       case _ : MethodType | PolyType  =>
               //  in such case, we can't transform tree to expr
               //  without eta-expansion.  
               //    from other side - we don't want do eta-expand now, it can be performed early.
                runRootUneta(term)
       case _ =>
                val expr = term.seal
                val monad = cpsCtx.asyncMonad
                expr match {
                  case '{ $e: $et } =>
                     val r = Async.rootTransform(e, monad, false)
                     if (r.haveAwait) 
                        val transformed = r.transformed.unseal
                        AwaitCpsTree(transformed, r.origin.unseal.tpe)
                     else
                        PureCpsTree(r.origin.unseal)
                     
                }
     }


  def runRootUneta(term: qctx.tasty.Term): CpsTree = {
     val monad = cpsCtx.asyncMonad
     term match {
       case Select(qual, name) =>
           runRoot(qual) match 
              case rq: AsyncCpsTree =>
                  val cTransformed = rq.transformed.asInstanceOf[qctx.tasty.Term]
                  CpsTree.impure(Select(cTransformed,term.symbol),term.tpe)
              case _: PureCpsTree =>
                  CpsTree.pure(term)
       case Ident(name) =>
             CpsTree.pure(term)
       case Apply(x, args) =>
             runApply(term,x,args)
       case _ =>
             throw MacroError(s"cps tree transform is not supported yet to ${term}",cpsCtx.patternCode)
     }
  }



